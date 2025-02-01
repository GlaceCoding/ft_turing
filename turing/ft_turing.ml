(* Définition d'une exception personnalisée *)
exception FileNotFound of string
exception InvalidJson of string
exception NullValue of string
exception MatchFailure of string
exception InvalidArgs of string
exception InfinityLoop of string

(* Importation de la librairie Str *)

(* Importation de la librairie Yojson *)
open Yojson.Basic

(* Fonction qui lit un fichier JSON et le parse *)
let read_json_file filename =
  try
    let json = Yojson.Basic.from_file filename in
    json
  with
  | Sys_error msg -> raise (FileNotFound ("File not found: " ^ filename))
  | Yojson.Json_error msg -> raise (InvalidJson ("Invalid JSON in file: " ^ filename))

(* Fonction qui extrait une valeur spécifique du JSON *)
let get_value_from_json json key =
  match json with
  | `Assoc assoc_list ->
      (try
        match List.assoc key assoc_list with
        | `Null -> raise (NullValue ("Null value for key: " ^ key))
        | value -> value
      with Not_found -> raise (NullValue ("Key not found: " ^ key)))
  | _ -> raise (InvalidJson "Expected JSON object")

(* Fonction qui vérifie la valeur name dans le JSON *)
let check_name json =
  match get_value_from_json json "name" with
  | `String name -> Printf.printf "***************************************************\n";
  Printf.printf "*                                                 *\n";
  Printf.printf "*                    %s                    *\n" name;
  Printf.printf "*                                                 *\n";
  Printf.printf "***************************************************\n";
  | _ -> raise (InvalidJson "Expected string for key 'name'")

(* Fonction qui vérifie la valeur alphabet dans le JSON et que les caracteres de la liste
 d'argument soient dans la liste des caracteres du json *)
let check_alphabet json argument =
  Printf.printf "Alphabet: [";
  match get_value_from_json json "alphabet" with
  | `List alphabet ->
      let result = List.map (fun x ->
        match x with
        | `String s when String.length s > 1 ->
          raise (InvalidJson ("Alphabet must be single characters: " ^ s))
        | `String s -> s
        | _ -> raise (InvalidJson "Alphabet must be strings")
      ) alphabet in
      Printf.printf "%s]\n" (String.concat ", " result);
      List.iter (fun x ->
        if not (List.mem x result) then
          raise (InvalidJson ("Argument not in alphabet: " ^ x))
      ) argument;
      alphabet
  | _ -> raise (InvalidJson "Expected list for key 'alphabet'")


(* Fonction auxiliaire qui vérifie si un caractère est dans l'alphabet *)
let is_in_alphabet c alphabet =
  List.exists (fun x ->
    match x with
    | `String s -> s = c
    | _ -> false
  ) alphabet

(* Convert Yojson alphabet list to string list *)
let convert_alphabet alphabet =
  List.map (fun x ->
    match x with
    | `String s -> s
    | _ -> raise (InvalidJson "Invalid alphabet format")
  ) alphabet


(* Fonction auxiliaire pour trouver l'index d'un élément dans une liste *)
let find_index element lst =
  let rec aux i = function
    | [] -> raise Not_found
    | h :: t -> if h = element then i else aux (i + 1) t
  in aux 0 lst

(* Fonction qui vérifie la valeur blank dans le JSON et retourne l'index de l'alphabet *)
let check_blank json alphabet =
  match get_value_from_json json "blank" with
  | `String blank when String.length blank = 1 ->
      let str_alphabet = convert_alphabet alphabet in
      if not (List.mem blank str_alphabet) then
        raise (InvalidJson ("Blank symbol must be in alphabet: " ^ blank))
      else (
        Printf.printf "Blank: %s\n" blank;
        (* find_index blank str_alphabet *)
        blank.[0]
      )
  | `String blank ->
      raise (InvalidJson ("Blank symbol must be a single character: " ^ blank))
  | _ -> raise (InvalidJson "Expected string for key 'blank'")

(* Fonction qui vérifie la valeur states dans le JSON *)
let check_states json =
  Printf.printf "States: [";
  match get_value_from_json json "states" with
  | `List states ->
      let result = List.map (fun x ->
        match x with
        | `String s -> s
        | _ -> raise (InvalidJson "States must be strings")
      ) states in
      Printf.printf "%s]\n" (String.concat ", " result);
      result
  | _ -> raise (InvalidJson "Expected list for key 'states'")

(* Fonction qui vérifie la valeur initial dans le JSON *)
let check_initial json states =
  match get_value_from_json json "initial" with
  | `String initial ->
      if not (List.mem initial states) then
        raise (InvalidJson ("Initial state must be in states: " ^ initial))
      else (
        Printf.printf "Initial : %s\n" initial;
        initial
      )
  | _ -> raise (InvalidJson "Expected string for key 'initial'")

(* Fonction qui vérifie la valeur final dans le JSON *)
let check_final json states =
  match get_value_from_json json "finals" with
  | `List finals ->
      let result = List.map (fun x ->
        match x with
        | `String s when List.mem s states ->
            Printf.printf "Finals : [ %s ]" s;
            s  (* Retourne la string au lieu de la valeur Yojson *)
        | `String s ->
            raise (InvalidJson ("Finals state must be in states: " ^ s))
        | _ ->
            raise (InvalidJson "Finals states must be strings")
      ) finals in
      Printf.printf "\n";
      result  (* Retourne une string list *)
  | _ -> raise (InvalidJson "Expected list for key 'finals'")

type action = LEFT | RIGHT

type transition = {
  read: string;
  to_state: string;
  write: string;
  action: string;
}

(* Fonction qui affiche les transitions *)
let display_transtions transitions_map =
  List.iter (fun (state, transitions) ->
    List.iter (fun transition ->
      Printf.printf "(%s, " state;
      Printf.printf "%s) -> " transition.read;
      Printf.printf "(%s, " transition.to_state;
      Printf.printf "%s, " transition.write;
      Printf.printf " %s)\n" transition.action;
    ) transitions
  ) transitions_map

(* Fonction qui vérifie la valeur transitions dans le JSON *)
let check_transitions json alphabet states =
  let str_alphabet = convert_alphabet alphabet in
  match get_value_from_json json "transitions" with
  | `Assoc transitions_map ->
      let verify_transition state transition =
        match transition with
        | `Assoc t -> (
            try
              let read = match List.assoc "read" t with
                | `String s when String.length s = 1 -> s
                | _ -> raise (InvalidJson "Invalid read value")
              in
              let to_state = match List.assoc "to_state" t with
                | `String s -> s
                | _ -> raise (InvalidJson "Invalid to_state value")
              in
              let write = match List.assoc "write" t with
                | `String s when String.length s = 1 -> s
                | _ -> raise (InvalidJson "Invalid write value")
              in
              let action = match List.assoc "action" t with
                | `String s when s = "LEFT" || s = "RIGHT" -> s
                | _ -> raise (InvalidJson "Invalid action value")
              in
              if not (List.mem read str_alphabet) then
                raise (InvalidJson ("Read symbol not in alphabet: " ^ read));
              if not (List.mem write str_alphabet) then
                raise (InvalidJson ("Write symbol not in alphabet: " ^ write));
              if not (List.mem to_state states) then
                raise (InvalidJson ("To_state not in states: " ^ to_state));
              {read; to_state; write; action}
            with Not_found ->
              raise (InvalidJson "Missing required field in transition")
          )
        | _ -> raise (InvalidJson "Invalid transition format")
      in
      let verify_state_transitions state transitions =
        match transitions with
        | `List t -> List.map (verify_transition state) t
        | _ -> raise (InvalidJson "Invalid transitions list format")
      in
      List.map (fun (state, transitions) ->
        (state, verify_state_transitions state transitions)
      ) transitions_map
  | _ -> raise (InvalidJson "Expected object for transitions")

(* Fonction qui aglomere toutes les fonctions de vérification *)
let check_json json argument =
  check_name json;
  let alphabet = check_alphabet json argument in
  let blank = check_blank json alphabet in
  let states = check_states json in
  let initial = check_initial json states in
  let finals = check_final json states in
  let transitions = check_transitions json alphabet states in
  display_transtions transitions;
  Printf.printf "***************************************************\n";
  (alphabet, blank, states, initial, finals, transitions)


(* Fonction qui affiche le contenu de la bande *)
let display_tape tape position blank =
  Printf.printf "[";
  let size = String.length tape in
  for i = 0 to size do
    let char = if i = size then blank else tape.[i] in
    if i = position then
      Printf.printf "<%c>" char
    else
      Printf.printf "%c" char
  done;
  for _ = 1 to 22 - size do
    Printf.printf "%c" blank 
  done;
  Printf.printf "] "

(* Fonction qui affiche les instructions *)
let display_transtion state transition =
  Printf.printf "(%s, " state;
  Printf.printf "%s) -> " transition.read;
  Printf.printf "(%s, " transition.to_state;
  Printf.printf "%s, " transition.write;
  Printf.printf " %s)\n" transition.action

(* Type pour représenter une configuration *)
type configuration = {
  state: string;
  position: int;
  tape: string;
}

(* Fonction qui lance la machine de Turing *)
let compute_turing_machine valid_setup argument =
  let alphabet, blank, states, initial, finals, transitions = valid_setup in
  (* Printf.printf "tape length: %d\n" (String.length tape); *)
  (* Set pour stocker les configurations vues *)
  let seen_configs = Hashtbl.create 1024 in
  let rec compute_turing_machine_aux tape state position =
    (* Créer la configuration actuelle *)
    let current_config = {
      state = state;
      position = position;
      tape = tape
      } in
      Unix.sleepf 0.2;
      flush stdout;
      (* Printf.printf "position: %d\n" position; *)
      (* Vérifier si cette configuration a déjà été vue *)
      if Hashtbl.mem seen_configs current_config then
        raise (InfinityLoop "Infinite loop detected!")
      else
        (* Ajouter la configuration courante *)
        Hashtbl.add seen_configs current_config ();
        if List.mem state finals then
          let trim_start str =
            let len = String.length str in
            let rec aux i = if i < len && str.[i] = ' ' then aux (i + 1) else i in
            let start = aux 0 in
            String.sub str start (len - start)
          in
          let trimmed_tape = (tape
            |> String.mapi (fun _ c -> if c = blank then ' ' else c) |> trim_start) in
          Printf.printf "Result: %s\nFinal state reached: %s\n" trimmed_tape state
        else
          let current_symbol = String.make 1 (if position < 0 || position >= String.length tape then
            blank
          else
            tape.[position]
          ) in
      try
        let transition = List.find (fun (s, _) -> s = state) transitions in
        let transition = List.find (fun t -> t.read = current_symbol) (snd transition) in
      let new_tape = if position < 0 then
        begin
          transition.write ^ tape
        end
      else if position >= String.length tape then
        tape ^ transition.write
      else
        let left = String.sub tape 0 position in
        let right = String.sub tape (position + 1) (String.length tape - position - 1) in
        left ^ transition.write ^ right
      in
      let new_position = if transition.action = "LEFT" then
        position - 1
      else if position < 0 then
        position + 2
      else
        position + 1
      in
      (* Printf.printf "Position: %d\n" position; *)
      display_tape tape position blank;
      display_transtion state transition;
      compute_turing_machine_aux new_tape transition.to_state new_position
    with
    | Not_found ->
      raise ( MatchFailure (Printf.sprintf "%s n'a pas d'instruction pour %s" state current_symbol) );
  in
  let tape = String.concat "" argument in
  compute_turing_machine_aux tape initial 0

let display_help () =
  Printf.printf "Usage: ./ft_turing [-h] jsonfile input\n\n";
  Printf.printf "positional arguments:\n jsonfile\t json description of the machine\n input\t\tinput of the machine\n\n";
  Printf.printf "optional arguments:\n -h, --help\t show this help message and exit\n"

(* Fonction principale *)
let () =
try
  let args = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  if Array.length Sys.argv < 3 then
    raise (InvalidArgs "Missing argument")
  else if Array.exists (fun arg -> arg = "-h" || arg = "--help") args then
    display_help ()
  else
    let filename = Sys.argv.(1) in
    let argument = Str.split (Str.regexp "") Sys.argv.(2) in
      Printf.printf "Argument: %s\n" (String.concat "," argument);
      let json = read_json_file filename in
      let valid_setup = check_json json argument in

      compute_turing_machine valid_setup argument;

  with
  | FileNotFound msg -> Printf.printf "Error: %s\n" msg
  | InvalidJson msg -> Printf.printf "Error: %s\n" msg
  | NullValue msg -> Printf.printf "Error: %s\n" msg
  | MatchFailure msg -> Printf.printf "Error : %s\n" msg
  | InvalidArgs msg -> display_help ()
  | InfinityLoop msg -> Printf.printf "Error: %s\n" msg

