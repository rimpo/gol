open Board
let colors v =  
  match v with 
    | Dead -> Raylib.Color.gray
    | Alive -> Raylib.Color.red

let setup size =
  Raylib.init_window (size * 40+ 100) (size * 40+ 100) "game of life";
  Raylib.set_target_fps 1;
  let b = Array.make (size * size) Dead in 
    mark_alive b size [24; 25; 26; 27; 28; 29; 30; 31]; (* single row *)
    b

let rec loop b size =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    clear_background Color.darkgray;
      for i = 0 to (size * size)-1 do
        let (x, y) = get_position i size in
          draw_rectangle (x*40+50) (y*40+50) 40 40 (colors b.(i))  
      done;

    next b size;
    end_drawing ();
    loop b size

let () =  loop (setup 8) 8