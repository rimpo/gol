open OUnit2
open Board
let test_get_index _ = 
  assert_equal 0 (get_index (0, 0) 4);
  assert_equal 1 (get_index (1, 0) 4);
  assert_equal 2 (get_index (2, 0) 4);
  assert_equal 3 (get_index (3, 0) 4);
  assert_equal 4 (get_index (0, 1) 4);
  assert_equal 5 (get_index (1, 1) 4);
  assert_equal 6 (get_index (2, 1) 4)


let test_mark_alive _ =
  let size = 3 in
  let lst = [1; 3; 5; 7] in
  let b = Array.make (size * size) Dead in
    mark_alive b size lst;
    assert_equal Alive b.(1);
    assert_equal Dead b.(2);
    assert_equal Alive b.(3);
    assert_equal Alive b.(5);
    assert_equal Dead b.(6);
    assert_equal Alive b.(7)

let test_count_neighbours_cell _ =
  let lst = [|Dead; Alive; Dead; Alive; Dead; Alive; Dead; Alive; Dead|] in 
  assert_equal 0 (count_if_alive lst 0 0 3);
  assert_equal 0 (count_if_alive lst 1 1 3);
  assert_equal 0 (count_if_alive lst 0 1 3);
  assert_equal 0 (count_if_alive lst 2 2 3)


let suite =
  "suite" >::: [
    "test_get_index" >:: test_get_index;
    "test_mark_alive" >:: test_mark_alive;
    "test_count_neighbours_cell" >:: test_count_neighbours_cell;
  ]
  
let () =
  run_test_tt_main suite