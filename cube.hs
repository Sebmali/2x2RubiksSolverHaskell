module Cube
(
    Cube,
    empty_cube,
    check_initial_cube,
    add_corner,
    solve_outer_cube
) where

import Corner
import Constants
import qualified Data.Map as Map

empty_cube :: Cube
add_corner :: Corner -> Cube -> Cube
check_initial_cube :: Cube -> Bool
solve_outer_cube :: Cube -> Cube -> Solution
solve_cube :: Cube -> Depth -> Depth -> Move -> Path -> Memo -> Result
try_moves :: Cube -> Depth -> Depth -> Move -> Path -> Moves -> Memo -> Result
mess_cube :: Cube -> Depth -> Depth -> Move -> Path -> Memo -> Memo -> Result
try_moves_2 :: Cube -> Depth -> Depth -> Move -> Path -> Moves -> Memo -> Memo -> Result
find_connecting_path :: Cube -> Depth -> Depth -> Move -> Key -> Path -> Memo -> Memo -> Result
try_moves_3 :: Cube -> Depth -> Depth -> Move -> Key -> Path -> Moves -> Memo -> Memo -> Result
update_memo :: Key -> Memo -> Depth -> Memo
move :: Cube -> Int -> Int -> [Int] -> Cube
apply_move :: Cube -> Move -> Cube
prune_moves :: Move -> Moves
convert_moves :: Moves -> Moves
convert_move :: Move -> Move
move_map :: Map.Map Move (Cube -> Cube)
cube_state_to_key :: Cube -> Key
corner_dissection :: Map.Map Char Int -> Int -> Key -> [Char] -> String

empty_cube = Cube []

add_corner corner (Cube corners) = Cube (corner:corners)

check_initial_cube (Cube corners) = length corners == 8

solve_outer_cube cube final_cube = solve_at_depth 1 where 
    solve_at_depth :: Depth -> Solution
    solve_at_depth depth 
        | depth > max_depth_limit = []
        | otherwise = 
            case solve_cube cube 0 depth "" [] Map.empty of 
                (True, path, _, _) -> path
                (False, _, memo, _) -> 
                    case mess_cube final_cube 0 depth "" [] memo Map.empty of
                        (True, path, _, key) -> 
                            case find_connecting_path cube 0 depth "" key [] memo Map.empty of
                                (True, second_path, _, _) -> second_path ++ (convert_moves path)
                                (False, _, _, _) -> solve_at_depth (depth + 1)
                        (False, _, _, _) -> solve_at_depth (depth + 1)

solve_cube cube curr_depth max_depth last_move path memo 
    | curr_depth >= max_depth = (False, [], memo, "")
    | Map.lookup cube_key memo == Just curr_depth = (False, [], memo, "")
    | otherwise = try_moves cube curr_depth max_depth last_move path (prune_moves last_move) new_memo
    where cube_key = cube_state_to_key cube
          new_memo = update_memo cube_key memo curr_depth

try_moves _ _ _ _ _ [] memo = (False, [], memo, "")
try_moves cube curr_depth max_depth last_move path (curr_move:moves) memo = do
    let new_cube = apply_move cube curr_move
    let result = solve_cube new_cube (curr_depth + 1) max_depth curr_move (curr_move:path) memo
    case result of 
        (True, _, _, _) -> result
        (False, _, next_memo, _) -> try_moves cube curr_depth max_depth last_move path moves next_memo

mess_cube cube curr_depth max_depth last_move path memo curr_memo
    | curr_depth >= max_depth = (False, [], memo, "")
    | Map.member cube_key memo = (True, path, memo, cube_key)
    | Map.lookup cube_key curr_memo == Just curr_depth = (False, [], memo, "")
    | otherwise = try_moves_2 cube curr_depth max_depth last_move path (prune_moves last_move) memo new_memo
    where cube_key = cube_state_to_key cube
          new_memo = update_memo cube_key curr_memo curr_depth

try_moves_2 _ _ _ _ _ [] memo _ = (False, [], memo, "")
try_moves_2 cube curr_depth max_depth last_move path (curr_move:moves) memo curr_memo = do
    let new_cube = apply_move cube curr_move
    let result = mess_cube new_cube (curr_depth + 1) max_depth curr_move (curr_move:path) memo curr_memo
    case result of 
        (True, _, _, _) -> result
        (False, _, new_memo, _) -> try_moves_2 cube curr_depth max_depth last_move path moves new_memo curr_memo

find_connecting_path cube curr_depth max_depth last_move key path memo curr_memo 
    | curr_depth >= max_depth = (False, [], memo, "")
    | current_key == key = (True, path, memo, current_key)
    | Map.lookup current_key curr_memo == Just curr_depth = (False, [], memo, "")
    | otherwise = try_moves_3 cube curr_depth max_depth last_move key path (prune_moves last_move) memo new_memo
    where current_key = cube_state_to_key cube
          new_memo = update_memo current_key curr_memo curr_depth

try_moves_3 _ _ _ _ _ _ [] memo _ = (False, [], memo, "")
try_moves_3 cube curr_depth max_depth last_move key path (curr_move:moves) memo curr_memo = do
    let new_cube = apply_move cube curr_move
    let result = find_connecting_path new_cube (curr_depth + 1) max_depth curr_move key (path ++ [curr_move]) memo curr_memo
    case result of 
        (True, _, _, _) -> result
        (False, _, new_memo, _) -> try_moves_3 cube curr_depth max_depth last_move key path moves new_memo curr_memo

update_memo key memo curr_depth = 
    case Map.lookup key memo of 
        Just value -> if curr_depth < value then Map.insert key curr_depth memo else memo
        Nothing -> Map.insert key curr_depth memo

move (Cube corners) a b indices_to_swap =
    let temp = swap_colors a b (corners !! (indices_to_swap !! 0))
        replace idx new_corn cube_state = take idx cube_state ++ [new_corn] ++ drop (idx + 1) cube_state
        cube1 = replace (indices_to_swap !! 0) (swap_colors a b (corners !! (indices_to_swap !! 1))) corners
        cube2 = replace (indices_to_swap !! 1) (swap_colors a b (cube1 !! (indices_to_swap !! 2))) cube1
        cube3 = replace (indices_to_swap !! 2) (swap_colors a b (cube2 !! (indices_to_swap !! 3))) cube2
        final_cube = replace (indices_to_swap !! 3) temp cube3
    in Cube final_cube

apply_move corners m = case Map.lookup m move_map of
    Just moveFunc -> moveFunc corners
    Nothing       -> corners

move_map = Map.fromList [
    (rvu, \cube -> move cube c1 c2 rvu_ind),
    (rvd, \cube -> move cube c1 c2 rvd_ind),
    (lvu, \cube -> move cube c1 c2 lvu_ind),
    (lvd, \cube -> move cube c1 c2 lvd_ind),
    (thr, \cube -> move cube c1 c3 thr_ind),
    (thl, \cube -> move cube c1 c3 thl_ind),
    (bhr, \cube -> move cube c1 c3 bhr_ind),
    (bhl, \cube -> move cube c1 c3 bhl_ind),
    (fc, \cube -> move cube c2 c3 fc_ind),
    (fcc, \cube -> move cube c2 c3 fcc_ind),
    (bc, \cube -> move cube c2 c3 bc_ind),
    (bcc, \cube -> move cube c2 c3 bcc_ind)
    ]

prune_moves m = case Map.lookup m pruned_move_map of
    Just moves -> moves
    Nothing    -> all_moves

convert_moves [] = []
convert_moves (m:ms) = convert_move m : convert_moves ms

convert_move m = case Map.lookup m convert_move_map of
    Just move -> move
    Nothing   -> m

cube_state_to_key (Cube corners) = 
    corner_dissection Map.empty 1 "" (get_all_colors corners)

corner_dissection _ _ key [] = key
corner_dissection key_dict position key (c:cs) = 
    case Map.lookup c key_dict of 
        Just value -> corner_dissection key_dict position (show value ++ key) cs
        Nothing -> corner_dissection (Map.insert c position key_dict) (position + 1) (show position ++ key) cs