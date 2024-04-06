{-
cube.hs
Purpose: This file contains the Cube class which is used to represent the Rubik's Cube. 
It contains methods to solve the cube and manipulate the cube.
Authors: Sebastian Maliczewski, Shayne Prakash
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
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
import Data.ByteString (count)
import Data_Types

empty_cube :: Cube
add_corner :: Corner -> Cube -> Cube
check_initial_cube :: Cube -> Bool
solve_outer_cube :: Cube -> Cube -> Solution
solve_cube :: Cube -> Depth -> Depth -> Move -> Path -> Memo -> Result
try_moves :: Cube -> Depth -> Depth -> Move -> Path -> Moves -> Memo -> Memo -> Key -> Int -> Result
mess_cube :: Cube -> Depth -> Depth -> Move -> Path -> Memo -> Memo -> Result
find_connecting_path :: Cube -> Depth -> Depth -> Move -> Key -> Path -> Memo -> Memo -> Result
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

check_initial_cube (Cube corners) = check_initial_cube_helper (Cube corners) 0 where{-&& appears_four_times (Cube corners) where-}
    check_initial_cube_helper :: Cube -> Int -> Bool
    check_initial_cube_helper _ 8 = True
    check_initial_cube_helper (Cube corners) index = if (corners !! index) `elem` initial_possibilities then check_initial_cube_helper (Cube corners) (index + 1) else False

count_occurrences :: Corner -> Cube -> Int -> Int
count_occurrences corner (Cube corners) index = count_occurrences_helper corner (Cube corners) index 0 where
    count_occurrences_helper :: Corner -> Cube -> Int -> Int -> Int
    count_occurrences_helper _ _ 8 count = count
    count_occurrences_helper corner (Cube corners) index count = if corner == (corners !! index) then count_occurrences_helper corner (Cube corners) (index + 1) (count + 1) else count_occurrences_helper corner (Cube corners) (index + 1) count

{-
This function handles the solving of the cube. It solves it using a depth first search algorithm
and starts at the unsolved cube and tries to solve it while also starting from the final cube
and messing it up to try and meet in the middle (Bi-directional search). Once a common key (cube)
is found, the final cubes path is connected with the path found from find_connecting_path to
create the solution.
-}
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

--Attempts to solve the given cube at the current depth
solve_cube cube curr_depth max_depth last_move path memo 
    | curr_depth >= max_depth = (False, [], memo, "")
    | Map.lookup cube_key memo == Just curr_depth = (False, [], memo, "") --Look if instance of cube is in memo
    | otherwise = try_moves cube curr_depth max_depth last_move path (prune_moves last_move) new_memo Map.empty "" 1
    where cube_key = cube_state_to_key cube
          new_memo = update_memo cube_key memo curr_depth

--Tries all possible moves at the current depth
try_moves _ _ _ _ _ [] memo _ _ _ = (False, [], memo, "")
try_moves cube curr_depth max_depth last_move path (curr_move:moves) memo curr_memo key choice = do
    let new_cube = apply_move cube curr_move
    let result = if choice == 1
                    then solve_cube new_cube (curr_depth + 1) max_depth last_move (curr_move:path) memo
                    else if choice == 2 then mess_cube new_cube (curr_depth + 1) max_depth last_move (curr_move:path) memo curr_memo
                    else find_connecting_path new_cube (curr_depth + 1) max_depth curr_move key (path ++ [curr_move]) memo curr_memo
    case result of 
        (True, _, _, _) -> result
        (False, _, next_memo, _) -> try_moves cube curr_depth max_depth last_move path moves next_memo curr_memo key choice

--This function messes up the final cube to try and meet in the middle
mess_cube cube curr_depth max_depth last_move path memo curr_memo
    | curr_depth >= max_depth = (False, [], memo, "")
    | Map.member cube_key memo = (True, path, memo, cube_key)
    | Map.lookup cube_key curr_memo == Just curr_depth = (False, [], memo, "")
    | otherwise = try_moves cube curr_depth max_depth last_move path (prune_moves last_move) memo new_memo "" 2
    where cube_key = cube_state_to_key cube
          new_memo = update_memo cube_key curr_memo curr_depth

--This function finds the connecting path between the final cube path and the initial cube path
find_connecting_path cube curr_depth max_depth last_move key path memo curr_memo 
    | curr_depth >= max_depth = (False, [], memo, "")
    | current_key == key = (True, path, memo, current_key)
    | Map.lookup current_key curr_memo == Just curr_depth = (False, [], memo, "")
    | otherwise = try_moves cube curr_depth max_depth last_move path (prune_moves last_move) memo new_memo key 3
    where current_key = cube_state_to_key cube
          new_memo = update_memo current_key curr_memo curr_depth

--This function updates the memo only if the current depth is less than the value in the memo, or doesn't exist.
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

--Map of all possible move function combinations, including colors, and indeces. 
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

--Converts the moves of the final cube moveset to their converse moves as it's backward.
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