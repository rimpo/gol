open Board

let cell_width = 5
let size = 128
let colors v =  
  match v with 
    | Dead -> Raylib.Color.gray
    | Alive -> Raylib.Color.red

let setup size =
  Raylib.init_window ((size * cell_width) + 100) ((size * cell_width) + 100) "game of life";
  Raylib.set_target_fps 10;
  let b = Array.make (size * size) Dead in 
    mark_alive b size (form_list size); 
    b

let rec loop b size =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    clear_background Color.darkgray;
      for i = 0 to (size * size)-1 do
        let (x, y) = get_position i size in
          draw_rectangle (x*cell_width+50) (y*cell_width+50) cell_width cell_width (colors b.(i))  
      done;

    next b size;
    end_drawing ();
    loop b size

let () =  loop (setup size) size