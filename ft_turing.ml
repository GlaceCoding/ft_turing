(* Définition d'une exception personnalisée *)
exception FileNotFound of string
exception InvalidJson of string
exception NullValue of string



(* Exemple d'utilisation *)


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
  | `String name -> Printf.printf "Name: %s\n" name
  | _ -> raise (InvalidJson "Expected string for key 'name'")

(* Fonction qui vérifie la valeur alphabet dans le JSON et que les caracteres de la liste
 d'argument soient dans la liste des caracteres du json *)
let check_alphabet json argument =
  match get_value_from_json json "alphabet" with
  | `List alphabet ->
      let result = List.map (fun x ->
        match x with
        | `String s when String.length s > 1 ->
          raise (InvalidJson ("Alphabet must be single characters: " ^ s))
        | `String s -> Printf.printf "%s " s; s
        | _ -> raise (InvalidJson "Alphabet must be strings")
      ) alphabet in
      Printf.printf "\n";
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
        find_index blank str_alphabet
      )
  | `String blank ->
      raise (InvalidJson ("Blank symbol must be a single character: " ^ blank))
  | _ -> raise (InvalidJson "Expected string for key 'blank'")

(* Fonction qui vérifie la valeur states dans le JSON *)
let check_states json =
  match get_value_from_json json "states" with
  | `List states ->
      let result = List.map (fun x ->
        match x with
        | `String s ->
            Printf.printf "%s " s;
            s
        | _ -> raise (InvalidJson "States must be strings")
      ) states in
      Printf.printf "\n";
      result
  | _ -> raise (InvalidJson "Expected list for key 'states'")

(* Fonction qui vérifie la valeur initial dans le JSON *)
let check_initial json states =
  match get_value_from_json json "initial" with
  | `String initial ->
      if not (List.mem initial states) then
        raise (InvalidJson ("Initial state must be in states: " ^ initial))
      else (
        Printf.printf "Initial state: %s\n" initial;
        initial
      )
  | _ -> raise (InvalidJson "Expected string for key 'initial'")

(* Fonction qui vérifie la valeur final dans le JSON *)
let check_final json states =
  match get_value_from_json json "finals" with
  | `List finals ->
      List.iter (fun x ->
        match x with
        | `String s when List.mem s states -> Printf.printf "%s " s
        | `String s ->
            raise (InvalidJson ("Finals state must be in states: " ^ s))
        | _ ->
            raise (InvalidJson "Finals states must be strings")
      ) finals;
      Printf.printf "\n";
      finals
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
    Printf.printf "State: %s\n" state;
    List.iter (fun transition ->
      Printf.printf "  Read: %s\n" transition.read;
      Printf.printf "  To state: %s\n" transition.to_state;
      Printf.printf "  Write: %s\n" transition.write;
      Printf.printf "  Action: %s\n" transition.action;
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
  let alphabet = check_alphabet json argument in
  let blank = check_blank json alphabet in
  let states = check_states json in
  let initial = check_initial json states in
  let finals = check_final json states in
  let transitions = check_transitions json alphabet states in
  display_transtions transitions;
  Printf.printf "JSON is valid\n";
  (alphabet, blank, states, initial, finals, transitions)

(* Fonction qui affiche l'instruction du caractere par rapport a l'etat actuel*)


(* Fonction principale *)
let () =
  let filename = "data.json" in
  (* split la chaine et transfome en liste *)
  try
    let argument = Str.split (Str.regexp "") Sys.argv.(1) in
    Printf.printf "Argument: %s\n" (String.concat "," argument);
    let json = read_json_file filename in
    let valid_setup = check_json json argument in

    (* display_instructions valid_setup argument; *)
    (* let instruction_list = map_instruction_list valid_setup argument in *)
    (* Printf.printf "Contenu du fichier JSON: %s\n" (Yojson.Basic.to_string json); *)

    (* Extraction d'une valeur spécifique *)
    let value = get_value_from_json json "alphabet" in
    Printf.printf "Valeur pour 'alphabet': %s\n" (Yojson.Basic.to_string value)
  with
  | FileNotFound msg -> Printf.printf "Erreur: %s\n" msg
  | InvalidJson msg -> Printf.printf "Erreur: %s\n" msg
  | NullValue msg -> Printf.printf "Erreur: %s\n" msg

