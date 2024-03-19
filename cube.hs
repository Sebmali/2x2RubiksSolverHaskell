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
    get_corner
) where

import Corner
import Constants
import Prelude 
import Debug.Trace

empty_cube :: Cube
add_corner :: Corner -> Cube -> Cube
check_initial_cube :: Cube -> Bool
get_corner :: Cube -> Int -> Corner
is_solved :: Cube -> Bool
is_side_solved :: Cube -> [Int] -> Int -> Bool
solve_outer_cube :: Cube -> [String]
solve_cube :: Cube -> Int -> Int -> String -> [String] -> (Bool, [String])
try_moves :: Cube -> Int -> Int -> String -> [String] -> [String] -> (Bool, [String])
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

solve_outer_cube cube = solveAtDepth 1 where
    solveAtDepth :: Int -> [String]
    solveAtDepth depth
      | depth > max_depth_limit = []
      | otherwise =
          case trace ("Depth: " ++ show depth) $ solve_cube cube 0 depth "" [] of
            (True, path) -> path
            (False, _) -> solveAtDepth (depth + 1)

solve_cube cube curr_depth max_depth lastMove path
    | is_solved cube = (True, path)
    | curr_depth >= max_depth = (False, [])
    | otherwise = try_moves cube curr_depth max_depth lastMove path (prune_moves lastMove)
        
try_moves _ _ _ _ _ [] = (False, []) -- No more moves to try
try_moves cube curr_depth max_depth last_move path (curr_move:moves) = do 
    let new_cube = apply_move cube curr_move
    let result = solve_cube new_cube (curr_depth + 1) max_depth curr_move (path ++ [curr_move])
    case result of
        (True, _) -> result
        (False, _) -> try_moves cube curr_depth max_depth last_move path moves

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