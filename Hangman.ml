exception Break

let print_list l =  List.iter (fun c->print_string c; print_string " ") l

let print_first_10 l =
  let rec aux l c = match l with 
    |[] -> ()
    |_ when c>=10 -> print_string "..."
    |t::q -> print_string t; print_string " "; aux q (c+1)
  in
    aux l 0 

(** Reads the text file [file] and returns a list of its lines in the form of a string list*)
let read_dict (file:string) : string list =
  let ic = open_in file in
  let l = ref [] in
  try (
    while true do
      let s = (input_line ic) in
      let s0 = String.sub s 0 (String.length s - 1) in
      l := s0 :: (!l)
    done;
    []
  ) with End_of_file -> close_in ic ;
  List.rev (!l)

(** Returns [true] whenever [c] is a letter of [w]*)
let in_letter (c:char) (word:string) : bool =
  try(
    for i=0 to (String.length word -1) do
      if word.[i]=c then raise Break
    done;
    false
  ) with _ -> true


(** Outputs the wardest possible configuration *)
let hardest (level:int) (remaining_words:string list) (letter:string) =
  (** Adds the pattern in the list of known patterns *)
  let rec update l p = match l with
    | []                   -> [(p,1)]
    |(p',c)::q when p' = p -> (p',c+1) :: q
    |(p',c)::q             -> (p',c)   :: (update q p) in
  (** Converts a word into a pattern *)
  let rec paternize l letter = match l with
    | []   -> []
    | t::q -> (
      let pattern = String.init level (fun i-> if t.[i]=letter.[0] then letter.[0] else '_') in
      pattern::(paternize q letter)
    ) in
  (** Outputs the pattern that encompasses the most words *)
  let rec find_hardest l candidate m = match l with 
    | [] -> candidate
    | (p',c)::q when c>m -> find_hardest q  p' c
    | (p',c)::q -> find_hardest q candidate m
  in
    let list_patterns = paternize remaining_words letter in
    let sorted_patterns = List.fold_left update [] list_patterns in
    let c = find_hardest sorted_patterns "" (-1) in
    assert (String.length c == level);
    c

let is_compatible w config letter = 
  assert (String.length w = String.length config);
  try (
  for i=0 to (String.length w - 1) do 
    match config.[i],w.[i] with
    | (c,w) when c=letter -> if w=letter then () else raise Break
    | (c,w) when c='_' -> if w=letter then raise Break else ()
    | _ -> ()
  done;
  true
  )
  with _ -> false 


let rec filter l (config:string) letter = match l with
  | []   -> []
  | t::q -> if is_compatible t config letter then t::(filter q config letter) else (filter q config letter)


let rec filter_size l n = match l with
  | []   -> []
  | t::q when String.length t = n -> t::(filter_size q n)
  | t::q -> filter_size q n

(** Insertion in a list, without repetition*)
let rec insert x l = match l with
  | [] -> [x]
  | t::q when t=x -> l
  | t::q -> t::(insert x q)

let hanged n word l =
let aux n = match n with
| 0 ->
  [|
  "";
  "         ";
  "         ";
  "";
  ""
  |]
| 1 ->
  [|
  "";
  "         ";
  "         ";
  "";
  "_______   "
  |]
| 2 ->
[|
  " |";
  " |       ";
  " |       ";
  " |";
  "_|_____   "
|]
| 3 ->
  [|
  " |";
  " |/       ";
  " |        ";
  " |";
  "_|_____   "
  |]
| 4 ->
[|
  " |------";
  " |/       ";
  " |        ";
  " |";
  "_|_____   "
|]
| 5->
  [|
    " |------| ";
    " |/       ";
    " |        ";
    " |";
    "_|_____   "
  |]
| 6 ->
  [|
  " |------| ";
  " |/     O ";
  " |        ";
  " |";
  "_|_____   "
  |]
| 7 ->
[|
  " |------| ";
  " |/     O ";
  " |      | ";
  " |";
  "_|_____   "
|]
| 8 ->
  [|
  " |------| ";
  " |/     O ";
  " |     /| ";
  " |";
  "_|_____   "
  |]
| 9 ->
[|
  " |------| ";
  " |/     O ";
  " |     /|\\";
  " |";
  "_|_____   "
|]
| 10 ->
  [|
  " |------| ";
  " |/     O ";
  " |     /|\\";
  " |     /";
  "_|_____   "
  |]
| 11 ->
[|
  " |------| ";
  " |/     O ";
  " |     /|\\";
  " |     / \\";
  "_|_____   "
|]
|_ -> failwith "P=NP"
in 
  Array.iteri (fun i x-> 
    (print_string x;
    if i = 1 then (
      print_string "  Word: ";
      print_bytes word;
    ) else if i = 2 then (
      print_string "  Bad letters: ";
      print_list l
    ) else ();
    print_newline()
    )) (aux n)

let victory word = not (in_letter '_' (Bytes.to_string word))

let knuth_shuffle a =
  let n = Array.length a in
  let a = Array.copy a in
  for i = n - 1 downto 1 do
    let k = Random.int (i+1) in
    let x = a.(k) in
    a.(k) <- a.(i);
    a.(i) <- x
  done;
  a

let clear() =
  let a = Sys.command("clear") in assert (a=a)

let () =
  Random.self_init();
  let word_list = ref (read_dict "dict.txt") in
  let level = 3 + Random.int (6+1) in
  word_list:= filter_size (!word_list) level;
  word_list:= Array.to_list (knuth_shuffle (Array.of_list (!word_list)));

  let tries = ref 0 in
  let bad_letters = ref [] in

  let word = Bytes.create level in
  for i=0 to (level-1) do 
    Bytes.set word i '_';
  done;

  let help_mode = true in

  let changed = ref false in
  while (not (victory word) && (!tries<11)) do
    clear();
    print_string "=== Impossible hangman ===\n";
    hanged (!tries) word !bad_letters;
    if help_mode then (
      print_newline();
      print_string ">>> ";
      print_first_10 (!word_list);
      print_string " <<<";
      print_newline();print_newline();
    );

    print_string "Next letter: ";
    print_newline();
    let user_input = In_channel.input_line In_channel.stdin in
    match user_input with
    | None -> ()
    | Some letter when String.length letter = 1 && List.exists (fun x-> x=letter) (!bad_letters) -> ()
    | Some letter when String.length letter = 1 -> (
      let config = hardest level (!word_list) letter in
      word_list := filter (!word_list) config letter.[0];
      changed:= false;
      for i=0 to (level-1) do 
        if config.[i]<>'_' then 
          (
            Bytes.set word i (config.[i]);
            changed := true;
          ) else ();
      done;
      if not (!changed) then (incr tries ; bad_letters:= (insert letter (!bad_letters)));
    )
    | Some bonus -> ()      
    done;
    clear();
    print_string "=== Impossible hangman ===\n";
    hanged (!tries) word !bad_letters;
    if (not (victory word)) then (
      print_string "You lost.";
    match (!word_list) with
      | [] -> failwith "Impossible"
      | t::q -> print_string (" The word was "^t^".\n")
    ) else (
      print_string ("You won! The word was indeed " ^ (Bytes.to_string word) ^ ".\n")
    )