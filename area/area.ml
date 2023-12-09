(* ------------------------------------------------------------- Modules ------------------------------------------------------------- *)

open Anacomb
open Bexp
open WhileB
open State

(* ------------------------------------------------------------- Exceptions ------------------------------------------------------------- *)

exception FileException of string
exception OptionException of string

(* ------------------------------------------------------------- Files Functions ------------------------------------------------------------- *)

let isDotArFile file_path = 
  match Filename.extension file_path with
  | ".ar" -> true
  | _ -> false

let isAreaProgramFile file_path =
  if Sys.file_exists file_path then
    if isDotArFile file_path then
      ()
    else
      raise (FileException ("the file " ^ file_path ^ " is not a .ar program."))
  else
    raise (FileException ("the file " ^ file_path ^ " does not exists."))

let read_line_option channel =
  try Some (input_line channel) with End_of_file -> None

let read_program_from_file file_path =
  let channel = open_in file_path in
  let rec read_lines acc =
    match read_line_option channel with
    | Some line -> read_lines (line :: acc)
    | None -> close_in channel; List.rev acc
  in
  read_lines []

(* ------------------------------------------------------------- Print Functions ------------------------------------------------------------- *)

let welcome () = Printf.printf "Area v1.0 (Dec 8 2023) on %s. Type \"help\" for more information. Type \"quit\" to stop programming.\n" Sys.os_type

let welcome_exec file_path = Printf.printf "Area v1.0 (Dec 8 2023) on %s. Running %s program...\n" Sys.os_type file_path

let welcome_interact file_path = Printf.printf "Area v1.0 (Dec 8 2023) on %s. Running %s program step-by-step... Press 'Enter' to advance the execution.\n\n" Sys.os_type file_path

let bye () = Printf.printf "Bye.\n"

let next () = Printf.printf ">>"

let help () = Printf.printf "Usage: ./area.byte                (to open the Area programming shell)\n    or ./area.byte <file_path>    (to execute an Area program)\n    or ./area.byte -i <file_path> (to execute an Area program step by step)\n"

let clear_screen () = Printf.printf "\027[2J\027[H"

let toCharList str =
  List.of_seq (String.to_seq str)

(* ------------------------------------------------------------- Print Functions ------------------------------------------------------------- *)

let rec infinite_loop () =
  Printf.printf ">> ";
  let command = read_line () in
  match command with
  | "quit" -> ()
  | "clear" -> clear_screen (); welcome (); infinite_loop ();
  | "help" -> let () = help () in infinite_loop ()
  | _ -> let p = pr_Programme (toCharList command) in
    let (a,s) = p in
    let s' = executer a [] in let () = Printf.printf "Final state: "; in
    print_interactive_result s';
    infinite_loop ()

let executeInteractiveProgram file_path = 
  let input_lines = read_program_from_file file_path in
  let input = String.concat "\n" input_lines in
  let prog = pr_Programme (list_of_string input) in
  let (a, s) = prog in
  let s' =  execution_interactive a 0 [] in ignore(s')

let executeProgram file_path = 
  let input_lines = read_program_from_file file_path in
  let input = String.concat "\n" input_lines in
  print_endline ("\n" ^ input ^ "\n");
  let prog = pr_Programme (list_of_string input) in
  let (a, s) = prog in
  let s' = executer a [] in let () = Printf.printf "Final state: "; in
  print_interactive_result s'

(* ------------------------------------------------------------- Options Functions ------------------------------------------------------------- *)

let optionI file_path = executeInteractiveProgram file_path

let switchOptions o file_path =
  match o with
  | "-i" -> optionI file_path
  | "--interactive" -> optionI file_path
  | _ -> raise (OptionException ("the option " ^ o ^ " does not exists."))

(* ------------------------------------------------------------- Main ------------------------------------------------------------- *)

let () =
  match Array.length Sys.argv with
  | 1 ->
    let () = clear_screen () in
    let () = welcome () in
    let () = infinite_loop () in 
    bye ()
  | 2 ->
    let file_path = Sys.argv.(1) in
    let () = welcome_exec file_path in
    (try
      isAreaProgramFile file_path;
      executeProgram file_path;
    with
    | FileException e -> Printf.printf "Error: FileException: %s\n" e;)
  | 3 -> 
    let file_path = Sys.argv.(2) in
    let () = welcome_interact file_path in
    (try
      isAreaProgramFile file_path;
      switchOptions Sys.argv.(1) file_path;
    with
    | FileException e -> Printf.printf "Error: FileException: %s\n" e;
    | OptionException e -> Printf.printf "Error: OptionException: %s\n" e;)
  | _ -> help ()
