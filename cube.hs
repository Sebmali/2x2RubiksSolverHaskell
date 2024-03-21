{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wall #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use list literal" #-}

module Cube
(
    Cube,
    empty_cube,
    check_initial_cube,
    add_corner,
    solve_outer_cube,
    get_corner,
    cube_state_to_key
) where

import Corner
import Constants
import Prelude 
import Debug.Trace
import qualified Data.Set as Set 

type Result = (Bool, [String], Set.Set (String, Int))

empty_cube :: Cube
add_corner :: Corner -> Cube -> Cube
check_initial_cube :: Cube -> Bool
get_corner :: Cube -> Int -> Corner
is_solved :: Cube -> Bool
is_side_solved :: Cube -> [Int] -> Int -> Bool
solve_outer_cube :: Cube -> [String]
solve_cube :: Cube -> Int -> Int -> String -> [String] -> Set.Set (String, Int) -> Result
try_moves :: Cube -> Int -> Int -> String -> [String] -> [String] -> Set.Set (String, Int) -> Result

move :: Cube -> Int -> Int -> [Int] -> Cube
apply_move :: Cube -> String -> Cube
prune_moves :: String -> [String]

data Cube = Cube [Corner]
    deriving (Show)

empty_cube = Cube []

add_corner corner (Cube corners) = Cube (corner:corners)

check_initial_cube (Cube corners) = length corners == 8

get_corner (Cube corners) x = corners !! x

is_solved cube = if is_side_solved cube front c1 && is_side_solved cube back c1 && 
                    is_side_solved cube top c2 && is_side_solved cube bottom c2 && 
                    is_side_solved cube right c3 && is_side_solved cube left c3 then True else False

is_side_solved (Cube corners) indeces color_index = 
    let cube_corners = [get_corner (Cube corners) x | x <- indeces] 
        cube_colors = [get_colors corner !! color_index | corner <- cube_corners] 
        in all (== head cube_colors) (tail cube_colors)

solve_outer_cube cube = solve_at_depth 1 where 
    solve_at_depth :: Int -> [String]
    solve_at_depth depth 
        | depth > max_depth_limit = []
        | otherwise = 
            case trace ("Depth: " ++ show depth) $ solve_cube cube 0 depth "" [] Set.empty of 
                (True, path, _) -> path
                (False, _, _) -> solve_at_depth (depth + 1)

solve_cube cube curr_depth max_depth last_move path memo
    | Set.member (cube_key, curr_depth) memo = (False, [], memo) 
    | is_solved cube = (True, path, memo)
    | curr_depth >= max_depth = (False, [], memo)
    | otherwise =  try_moves cube curr_depth max_depth last_move path (prune_moves last_move) (Set.insert (cube_key, curr_depth) memo)
    where cube_key = cube_state_to_key cube

try_moves _ _ _ _ _ [] memo = (False, [], memo)
try_moves cube curr_depth max_depth last_move path (curr_move:moves) memo = do 
    let new_cube = apply_move cube curr_move 
    let result = solve_cube new_cube (curr_depth + 1) max_depth curr_move (path ++ [curr_move]) memo 
    case result of 
        (True, _, _) -> result 
        (False, _, new_memo) -> try_moves cube curr_depth max_depth last_move path moves new_memo 

move (Cube corners) a b indices_to_swap =
    let temp = swap1 a b (corners !! (indices_to_swap !! 0))
        replace idx new_corn cube_state = take idx cube_state ++ [new_corn] ++ drop (idx + 1) cube_state
        cube1 = replace (indices_to_swap !! 0) (swap1 a b (corners !! (indices_to_swap !! 1))) corners
        cube2 = replace (indices_to_swap !! 1) (swap1 a b (cube1 !! (indices_to_swap !! 2))) cube1
        cube3 = replace (indices_to_swap !! 2) (swap1 a b (cube2 !! (indices_to_swap !! 3))) cube2
        final_cube = replace (indices_to_swap !! 3) temp cube3
    in Cube final_cube

apply_move (Cube corners) m 
    | m == rvu = move (Cube corners) c1 c2 rvu_ind
    | m == rvd = move (Cube corners) c1 c2 rvd_ind
    | m == lvu = move (Cube corners) c1 c2 lvu_ind
    | m == lvd = move (Cube corners) c1 c2 lvd_ind
    | m == thr = move (Cube corners) c1 c3 thr_ind
    | m == thl = move (Cube corners) c1 c3 thl_ind
    | m == bhr = move (Cube corners) c1 c3 bhr_ind
    | m == bhl = move (Cube corners) c1 c3 bhl_ind
    | m == fc = move (Cube corners) c2 c3 fc_ind
    | m == fcc = move (Cube corners) c2 c3 fcc_ind
    | m == bc = move (Cube corners) c2 c3 bc_ind
    | m == bcc = move (Cube corners) c2 c3 bcc_ind
    | otherwise = Cube corners

prune_moves m 
    | m == rvu = rvu_moves
    | m == rvd = rvd_moves
    | m == lvu = lvu_moves
    | m == lvd = lvd_moves
    | m == thr = thr_moves
    | m == thl = thl_moves
    | m == bhr = bhr_moves
    | m == bhl = bhl_moves
    | m == fc = fc_moves
    | m == fcc = fcc_moves
    | m == bc = bc_moves
    | m == bcc = bcc_moves
    | otherwise = all_moves

cube_state_to_key :: Cube -> String
cube_state_to_key (Cube corners) = 
    let key_dict = []
        position = 1
        key = ""
        cube_colors = [get_colors corner | corner <- corners] 
        color_array = concat cube_colors
        in corner_dissection key_dict color_array key position

corner_dissection :: [(Char, Int)] -> [Char] -> String -> Int -> String
corner_dissection _ [] key _ = key 
corner_dissection [] (color:xs) key position = 
    let new_key_dict = [(color, position)]
        new_key = key ++ show position
    in corner_dissection new_key_dict xs new_key (position + 1)
corner_dissection key_dict (color:xs) key position = 
    case lookup color key_dict of
        Just pos ->
            let new_key = key ++ show pos
            in corner_dissection key_dict xs new_key position
        Nothing -> 
            let new_key_dict = key_dict ++ [(color, position)]
                new_key = key ++ show position
            in corner_dissection new_key_dict xs new_key (position + 1)