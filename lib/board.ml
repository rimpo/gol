type status =
  | Dead
  | Alive 

(* type position = int * int *)

let get_index pos size = (snd pos) * size + (fst pos) 

let get_position idx size = (idx mod size, idx / size)


let is_alive idx b = 
    match b.(idx) with
      | Alive -> true
      | Dead -> false

(*let is_position_alive pos b =  is_alive (get_index pos) b*)

(* count for each cell which are alive *)

let count_if_alive b x y size = 
  let i = get_index (x, y) size in
    match i with
    | x when -1 < x && x < (size * size)  -> if is_alive x b then 1 else 0
    | _ -> 0

let get_position_coordinate x y = 
  [(x-1, y-1); (x, y-1); (x+1, y-1);(x-1, y); (*(x, y);*) (x+1, y);(x-1, y+1); (x, y+1); (x+1,y+1)]


let count_neighbours_cell b x y size =
  let rec count_neighbours_cell_aux acc b lst size = 
      match lst with
        | [] -> acc
        | (x, y) :: tl -> count_neighbours_cell_aux (acc+(count_if_alive b x y size)) b tl size
  in count_neighbours_cell_aux 0 b (get_position_coordinate x y) size



let count_neighbours b i size= 
    let (x, y) = get_position i size in
    count_neighbours_cell b x y size
(* count the neighbours each cell has. edges and corners will have less neighbours *)
let neighbours b size = 
    let n = Array.make (size * size) 0 in
      for i = 0 to (size * size) - 1 do
        n.(i) <- count_neighbours b i size
      done;
    n

let evolve n b i =
    match b.(i) with
      | Dead -> if n.(i) = 3 then Alive else Dead
      | Alive -> 
          match n.(i) with
            | 2 | 3 -> Alive
            | _ -> Dead      

let next b size = 
    let n = neighbours b size in
      for i = 0 to (size * size)-1 do
        b.(i) <- evolve n b i
      done


let rec mark_alive b size lst = 
  match lst with
    | [] -> ()
    | h :: t -> b.(h) <- Alive; mark_alive b size t;