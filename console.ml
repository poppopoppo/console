let command_n = ref 0
let title_txt =
  "\
this is console
please write a command and hit Ctrl-s
---------------------------------------
"
(*
 * double_editor.ml
 * ----------
 * Copyright : (c) 2016, Fabian Bonk <fabian.bonk@bonkii.com>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)
open LTerm_geom

let ( >>= ) = Lwt.( >>= )

(* helper functions *)
let make_key ?(ctrl = false) ?(meta = false) ?(shift = false) c =
  let code =
    match c with
    | `Char c -> LTerm_key.Char (CamomileLibrary.UChar.of_char c)
    | `Other key -> key in
  { LTerm_key.control = ctrl; meta; shift; code }

let frame widget =
  let frame = new LTerm_widget.frame in
  frame#set widget;
  frame

let ctrl_ c = [make_key ~ctrl:true @@ `Char c]
let key_ c = [make_key (`Char c)]
let sp_key_ k = [make_key (`Other k)]
let insstr e s =
  [LTerm_edit.Custom (fun () ->
       Zed_edit.insert e#context (Zed_rope.of_string s))]
let echo e () =
  (Zed_edit.insert e#context (Zed_rope.of_string e#text))
let main () =
  let waiter, wakener = Lwt.wait () in
  let quit = [LTerm_edit.Custom (Lwt.wakeup wakener)] in

  let vbox = new LTerm_widget.vbox in

  let top_editor = new LTerm_edit.edit () in
  let top_frame = frame top_editor in

  let bottom_editor = new LTerm_edit.edit ~size:{ rows = 10; cols = 1 } () in
  bottom_editor#set_allocation
    { bottom_editor#allocation with row1 = bottom_editor#allocation.row1 - 5 };
  let bottom_frame = frame bottom_editor in

  let _ = vbox#add top_frame in
  let _ = vbox#add ~expand:false bottom_frame in

  let _ = bottom_editor#bind (ctrl_ 'c') quit in
  let hello = insstr bottom_editor "Hello" in
  let _ = editor#bind (ctrl_ 'c') quit in
  let _ = editor#bind (ctrl_ 'h') hello in
  let _ = editor#bind (ctrl_ 'e') [LTerm_edit.Custom (echo bottom_editor)] in
  let _ = Zed_edit.insert bottom_editor#context (Zed_rope.of_string title_txt) in

  let vscroll = new LTerm_widget.vscrollbar ~width:1 bottom_editor#vscroll in
  bottom_frame#add vscroll in

  Lazy.force LTerm.stdout
  >>= fun term ->
  LTerm.enable_mouse term
  >>= fun () ->
  Lwt.finalize
    (fun () -> LTerm_widget.run term ~save_state:false ~load_resources:false vbox waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (main ())
