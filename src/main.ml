open Lang
open Translator
open Vocab
open Options
open Verifier
open Report
open ItvDom
open Profiler
open Query

let set_default_inline_depth () =
  if !Options.inline_depth < 0 then Options.inline_depth := 2
  else ()

let prepare () =
  let _ = Z3Interface.ctx := Z3.mk_context [("timeout",string_of_int (!Options.z3timeout))] in
  let _ = set_default_inline_depth () in
  let json = Yojson.Basic.from_file !inputfile in
  let lines = BatList.of_enum (BatFile.lines_of !inputfile) in
  let (_,lst) = (* store cumulative byte size at the end of each line *) 
    List.fold_left (fun (acc_eol,acc_lst) cur ->
      let eol = Bytes.length (Bytes.of_string cur) + 1 in
      let acc_eol' = eol + acc_eol in
      (acc_eol', acc_lst @ [acc_eol'])
    ) (0,[]) lines in
  let _ = end_of_lines := lst in
  let pgm = Translator.run json in
  let _ = main_contract := get_cname (get_main_contract pgm) in
  let pgm = Preprocess.run pgm in
  let pgm = MakeCfg.run pgm in
  let pgm = Inline.run pgm in (* inlining is performed on top of an initial cfg. *)
  let pgm = CallGraph.remove_unreachable_funcs pgm in
  let global = Global.make_global_info pgm in
  (pgm, global, lines)

let main () =
  let (pgm,global,lines) = prepare () in
  if !Options.il then
    print_endline (to_string_pgm pgm) else
  if !Options.cfg then
    print_endline (to_string_cfg_p pgm)
  else
    let paths = Path.generate pgm in
    let mem = ItvAnalysis.run pgm global paths in
    let qmap = Verifier.run global mem paths in
    let _ = Report.print qmap in
    let _ = end_time := Sys.time () -. !Profiler.start_time in
    print_endline ("\nTime Elapsed : " ^ string_of_float !end_time)

let _ =
  let usageMsg = "./main.native -input filename" in
  Arg.parse options activate_detector usageMsg;
  activate_default_detector_if_unspecified ();
  print_detector_activation_status ();
  Printexc.record_backtrace true;
  try main ()
  with exc -> prerr_endline (Printexc.to_string exc); prerr_endline (Printexc.get_backtrace())
