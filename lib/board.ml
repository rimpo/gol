type status =
  | Dead
  | Alive

(* get_index converts from (x,y) to index
   example: (0, 1) -> 5  when size = 5
*)
let get_index pos size = (snd pos * size) + fst pos
let get_position idx size = idx mod size, idx / size

let is_alive idx b =
  match b.(idx) with
  | Alive -> true
  | Dead -> false
;;

(* count for each cell which are alive *)
let count_if_alive b x y size =
  let i = get_index (x, y) size in
  match i with
  | x when -1 < x && x < size * size -> if is_alive x b then 1 else 0
  | _ -> 0
;;

let get_neighbours x y =
  [ x - 1, y - 1
  ; x, y - 1
  ; x + 1, y - 1
  ; x - 1, y
  ; (*(x, y);*)
    x + 1, y
  ; x - 1, y + 1
  ; x, y + 1
  ; x + 1, y + 1
  ]
;;

let count_alive_neighbours b x y size =
  let rec count_alive_neighbours_aux acc b lst size =
    match lst with
    | [] -> acc
    | (x, y) :: tl ->
      count_alive_neighbours_aux (acc + count_if_alive b x y size) b tl size
  in
  count_alive_neighbours_aux 0 b (get_neighbours x y) size
;;

let count_neighbours b i size =
  let x, y = get_position i size in
  count_alive_neighbours b x y size
;;

(* form a board containing count of neighbour cells alive *)
let form_count_board b size =
  let n = Array.make (size * size) 0 in
  for i = 0 to (size * size) - 1 do
    n.(i) <- count_neighbours b i size
  done;
  n
;;

let evolve n b i =
  match b.(i) with
  | Dead -> if n.(i) = 3 then Alive else Dead
  | Alive ->
    (match n.(i) with
     | 2 | 3 -> Alive
     | _ -> Dead)
;;

(* next stage based on the cell evolution *)
let next b size =
  let n = form_count_board b size in
  for i = 0 to (size * size) - 1 do
    b.(i) <- evolve n b i
  done
;;

(* marking board with alive based on index in list *)
let rec mark_alive b size lst =
  match lst with
  | [] -> ()
  | h :: t ->
    b.(h) <- Alive;
    mark_alive b size t
;;

(* form list of cells which will be marked alive. It is going to be middle row of the table
   example for size=8 output list = [24; 25; 26; 27; 28; 29; 30; 31];
*)
let form_list size =
  let l = ref [] in
  for i = size / 2 * size to (size / 2 * size) + size - 1 do
    l := i :: !l
  done;
  !l
;;
