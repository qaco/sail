open Libsail

open Interactive.State

type version = { major : int; minor : int; patch : int }

(* Current version of Sail. Must be updated manually. CI checks this matches
   the tag given by `git describe`. *)
let version = { major = 0; minor = 17; patch = 1 }

let opt_free_arguments : string list ref = ref []
let opt_file_out : string option ref = ref None
let opt_just_check : bool ref = ref false
let opt_just_parse_project : bool ref = ref false
let opt_splice : string list ref = ref []
let opt_memo_z3 = ref true
let opt_all_modules = ref false
let opt_project_files : string list ref = ref []
let opt_variable_assignments : string list ref = ref []

(* This function does roughly the same thing as Arg.align, except we
   can call it per target and it won't add additional -help
   options. *)
let target_align opts =
  let split_doc doc =
    if String.length doc > 0 then
      if doc.[0] = ' ' then ("", doc)
      else if doc.[0] = '\n' then ("", doc)
      else (
        match String.index_from_opt doc 0 ' ' with
        | Some n -> (String.sub doc 0 n, String.sub doc n (String.length doc - n))
        | None -> ("", doc)
      )
    else ("", "")
  in
  let opts = List.map (fun (flag, spec, doc) -> (flag, spec, split_doc doc)) opts in
  let alignment = List.fold_left (fun a (flag, _, (arg, _)) -> max a (String.length flag + String.length arg)) 0 opts in
  let opts =
    List.map
      (fun (flag, spec, (arg, doc)) ->
        if doc = "" then (flag, spec, arg)
        else (flag, spec, arg ^ String.make (max 0 (alignment - (String.length flag + String.length arg))) ' ' ^ doc)
      )
      opts
  in
  opts

(* Add a header to separate arguments from each plugin *)
let add_target_header plugin opts =
  let add_header (flag, spec, doc) =
    let desc =
      match Target.extract_registered () with
      | [] -> "plugin " ^ plugin
      | [name] -> "target " ^ name
      | names -> "targets " ^ Util.string_of_list ", " (fun x -> x) names
    in
    (flag, spec, doc ^ " \n\nOptions for " ^ desc)
  in
  Util.update_last add_header opts

let load_plugin opts plugin =
  try
    Dynlink.loadfile_private plugin;
    let plugin_opts = Target.extract_options () |> target_align in
    opts := add_target_header plugin !opts @ plugin_opts
  with Dynlink.Error msg -> prerr_endline ("Failed to load plugin " ^ plugin ^ ": " ^ Dynlink.error_message msg)

(* Version as a string, e.g. "1.2.3". *)
let version_string = Printf.sprintf "%d.%d.%d" version.major version.minor version.patch

(* Full version string including Git branch & commit. *)
let version_full =
  let open Manifest in
  Printf.sprintf "Sail %s (%s @ %s)" version_string branch commit

let usage_msg = version_string ^ "\nusage: sail <options> <file1.sail> ... <fileN.sail>\n"

let help options = raise (Arg.Help (Arg.usage_string options usage_msg))

let rec options =
  ref
    [
      ("-o", Arg.String (fun f -> opt_file_out := Some f), "<prefix> select output filename prefix");
      ("-no_warn", Arg.Clear Reporting.opt_warnings, " do not print warnings");
      ("-all_warnings", Arg.Set Reporting.opt_all_warnings, " print all warning messages");
      ("-just_check", Arg.Set opt_just_check, " terminate immediately after typechecking");
      ("-verbose", Arg.Int (fun verbosity -> Util.opt_verbosity := verbosity), "<verbosity> produce verbose output");
      ("-help", Arg.Unit (fun () -> help !options), " display this list of options");
      ("--help", Arg.Unit (fun () -> help !options), " display this list of options");
    ]

let register_default_target () = Target.register ~name:"default" Target.empty_action

let file_to_string filename =
  let chan = open_in filename in
  let buf = Buffer.create 4096 in
  try
    let rec loop () =
      let line = input_line chan in
      Buffer.add_string buf line;
      Buffer.add_char buf '\n';
      loop ()
    in
    loop ()
  with End_of_file ->
    close_in chan;
    Buffer.contents buf

let run_sail (config : Yojson.Basic.t option) tgt =
  Target.run_pre_parse_hook tgt ();

  let project_files, frees =
    List.partition (fun free -> Filename.check_suffix free ".sail_project") !opt_free_arguments
  in

  let ctx, ast, env, effect_info =
    match (project_files, !opt_project_files) with
    | [], [] ->
        (* If there are no provided project files, we concatenate all
           the free file arguments into one big blob like before *)
        Frontend.load_files ~target:tgt Locations.sail_dir !options Type_check.initial_env frees
    (* Allows project files from either free arguments via suffix, or
       from -project, but not both as the ordering between them would
       be unclear. *)
    | project_files, [] | [], project_files ->
        let t = Profile.start () in
        let defs =
          List.map
            (fun project_file ->
              let root_directory = Filename.dirname project_file in
              let contents = file_to_string project_file in
              Project.mk_root root_directory :: Initial_check.parse_project ~filename:project_file ~contents ()
            )
            project_files
          |> List.concat
        in
        let variables = ref Util.StringMap.empty in
        List.iter
          (fun assignment ->
            if not (Project.parse_assignment ~variables assignment) then
              raise (Reporting.err_general Parse_ast.Unknown ("Could not parse assignment " ^ assignment))
          )
          !opt_variable_assignments;
        let proj = Project.initialize_project_structure ~variables defs in
        let mod_ids =
          if !opt_all_modules then Project.all_modules proj
          else
            List.map
              (fun mod_name ->
                match Project.get_module_id proj mod_name with
                | Some id -> id
                | None -> raise (Reporting.err_general Parse_ast.Unknown ("Unknown module " ^ mod_name))
              )
              frees
        in
        Profile.finish "parsing project" t;
        if !opt_just_parse_project then exit 0;
        let env = Type_check.initial_env_with_modules proj in
        Frontend.load_modules ~target:tgt Locations.sail_dir !options env proj mod_ids
    | _, _ ->
        raise
          (Reporting.err_general Parse_ast.Unknown
             "Module files (.sail_project) should either be specified with the appropriate option, or as free \
              arguments with the appropriate extension, but not both!"
          )
  in
  let ast, env = Frontend.initial_rewrite effect_info env ast in
  let ast, env = match !opt_splice with [] -> (ast, env) | files -> Splice.splice_files ctx ast (List.rev files) in
  let effect_info = Effects.infer_side_effects (Target.asserts_termination tgt) ast in

  (* Don't show warnings during re-writing for now *)
  Reporting.suppressed_warning_info ();
  Reporting.opt_warnings := false;

  Target.run_pre_rewrites_hook tgt ast effect_info env;
  let ctx, ast, effect_info, env = Rewrites.rewrite ctx effect_info env (Target.rewrites tgt) ast in

  Target.action tgt !opt_file_out { ctx; ast; effect_info; env; default_sail_dir = Locations.sail_dir; config };

  (ctx, ast, env, effect_info)

let get_plugin_dir () =
  match Sys.getenv_opt "SAIL_PLUGIN_DIR" with
  | Some path -> path :: Libsail_sites.Sites.plugins
  | None -> Libsail_sites.Sites.plugins

let main () =

  begin
    match get_plugin_dir () with
    | dir :: _ ->
       List.iter
         (fun plugin ->
           let path = Filename.concat dir plugin in
           if Filename.extension plugin = ".cmxs" then load_plugin options path
         )
         (Array.to_list (Sys.readdir dir))
    | [] -> ()
  end;

  Arg.parse_dynamic options (fun s -> opt_free_arguments := !opt_free_arguments @ [s]) usage_msg;

  let default_target = register_default_target () in

  (* let ctx, ast, env, effect_info = *)
  let _, _, _, _ =
    match Target.get_the_target () with
    | Some target when not !opt_just_check -> run_sail None target
    | _ -> run_sail None default_target
  in

  ()

let () =
  try try main () with Failure s -> raise (Reporting.err_general Parse_ast.Unknown s)
  with Reporting.Fatal_error e ->
    Reporting.print_error e;
    if !opt_memo_z3 then Constraint.save_digests () else ();
    exit 1
