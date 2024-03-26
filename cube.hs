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
<<<<<<< Updated upstream
=======
import qualified Data.Set as Set 
import qualified Data.Map as Map

type Result = (Bool, [String], Map.Map String Int, String)
>>>>>>> Stashed changes

empty_cube :: Cube
add_corner :: Corner -> Cube -> Cube
check_initial_cube :: Cube -> Bool
get_corner :: Cube -> Int -> Corner
is_solved :: Cube -> Bool
is_side_solved :: Cube -> [Int] -> Int -> Bool
<<<<<<< Updated upstream
solve_outer_cube :: Cube -> [String]
solve_cube :: Cube -> Int -> Int -> String -> [String] -> (Bool, [String])
try_moves :: Cube -> Int -> Int -> String -> [String] -> [String] -> (Bool, [String])
=======
>>>>>>> Stashed changes
move :: Cube -> Int -> Int -> [Int] -> Cube
apply_move :: Cube -> String -> Cube
prune_moves :: String -> [String]
convert_moves :: [String] -> [String]
convert_move :: String -> String

data Cube = Cube [Corner]
    deriving (Show)

empty_cube = Cube []

add_corner corner (Cube corners) = Cube (corner:corners)

check_initial_cube (Cube corners) = length corners == 8

get_corner (Cube corners) x = corners !! x

is_solved cube = is_side_solved cube front c1 && is_side_solved cube back c1 && 
                is_side_solved cube top c2 && is_side_solved cube bottom c2 && 
                is_side_solved cube right c3 && is_side_solved cube left c3

is_side_solved (Cube corners) indices color_index = 
    let cube_colors = map (\index -> (get_colors (get_corner (Cube corners) index)) !! color_index) indices
    in all (== head cube_colors) cube_colors

<<<<<<< Updated upstream
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
=======
solve_outer_cube :: Cube -> Cube -> [String]
solve_outer_cube cube final_cube = solve_at_depth 1 where 
    solve_at_depth :: Int -> [String]
    solve_at_depth depth 
        | depth > max_depth_limit = []
        | otherwise = 
            case trace ("Depth Cube: " ++ show depth) $ solve_cube cube 0 depth "" [] Map.empty of 
                (True, path, _, _) -> path
                (False, _, memo, _) -> 
                    case trace (show depth) $ mess_cube final_cube 0 depth "" [] memo of
                        (True, path, _, key) -> 
                            case trace ("Connecting!: " ++ show key) $ find_connecting_path cube 0 depth "" key [] memo of
                                (True, second_path, _, _) -> trace ("Connected.\n" ++ "Second_path: " ++ show second_path ++ "First_path: " ++ show path) $ second_path ++ (convert_moves (reverse path))
                                (False, _, _, _) -> trace ("Not connected") $ solve_at_depth (depth + 1)
                        (False, _, _, _) -> solve_at_depth (depth + 1)

solve_cube :: Cube -> Int -> Int -> String -> [String] -> Map.Map String Int -> Result
solve_cube cube curr_depth max_depth last_move path memo 
    | Map.lookup cube_key memo == Just curr_depth = (False, [], memo, "")
    | is_solved cube = (True, path, new_memo, cube_key)
    | curr_depth >= max_depth = (False, [], memo, "")
    | otherwise = try_moves cube curr_depth max_depth last_move cube_key path (prune_moves last_move) new_memo
    where cube_key = cube_state_to_key cube
          new_memo = Map.insert cube_key curr_depth memo
        
try_moves :: Cube -> Int -> Int -> String -> String -> [String] -> [String] -> Map.Map String Int -> Result
try_moves _ _ _ _ _ _ [] memo = (False, [], memo, "")
try_moves cube curr_depth max_depth last_move cube_key path (curr_move:moves) memo = do
    let new_cube = apply_move cube curr_move
    let result = solve_cube new_cube (curr_depth + 1) max_depth curr_move (path ++ [curr_move]) memo
    case result of 
        (True, _, _, _) -> result
        (False, _, next_memo, _) -> try_moves cube curr_depth max_depth last_move cube_key path moves next_memo

mess_cube :: Cube -> Int -> Int -> String -> [String] -> Map.Map String Int -> Result
mess_cube cube curr_depth max_depth last_move path memo
    | Map.member cube_key memo = trace ("Cube Key: " ++ show cube_key) $ (True, path, memo, cube_key)
    | curr_depth >= max_depth = (False, [], memo, "")
    | otherwise = try_moves_2 cube curr_depth max_depth last_move cube_key path (prune_moves last_move) memo
    where cube_key = cube_state_to_key cube

try_moves_2 :: Cube -> Int -> Int -> String -> String -> [String] -> [String] -> Map.Map String Int -> Result
try_moves_2 _ _ _ _ _ _ [] memo = (False, [], memo, "")
try_moves_2 cube curr_depth max_depth last_move cube_key path (curr_move:moves) memo = do
    let new_cube = apply_move cube curr_move
    let result = mess_cube new_cube (curr_depth + 1) max_depth curr_move (path ++ [curr_move]) memo
    case result of 
        (True, _, _, _) -> result
        (False, _, new_memo, _) -> try_moves_2 cube curr_depth max_depth last_move cube_key path moves new_memo

find_connecting_path :: Cube -> Int -> Int -> String -> String -> [String] -> Map.Map String Int -> Result
find_connecting_path cube curr_depth max_depth last_move key path memo
    | current_key == key = (True, path, memo, current_key)
    | curr_depth >= max_depth = (False, [], memo, "")
    | otherwise = try_moves_3 cube curr_depth max_depth last_move current_key key path (prune_moves last_move) memo
    where current_key = cube_state_to_key cube

try_moves_3 :: Cube -> Int -> Int -> String -> String -> String -> [String] -> [String] -> Map.Map String Int -> Result
try_moves_3 _ _ _ _ _ _ _ [] memo = (False, [], memo, "")
try_moves_3 cube curr_depth max_depth last_move current_key key path (curr_move:moves) memo = do
    let new_cube = apply_move cube curr_move
    let result = find_connecting_path new_cube (curr_depth + 1) max_depth curr_move key (path ++ [curr_move]) memo
    case result of 
        (True, _, _, _) -> result
        (False, _, new_memo, _) -> try_moves_3 cube curr_depth max_depth last_move current_key key path moves new_memo
>>>>>>> Stashed changes

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
<<<<<<< Updated upstream
    | otherwise = all_moves
=======
    | otherwise = all_moves

convert_moves [] = []
convert_moves (m:ms) = convert_move m : convert_moves ms

convert_move m 
    | m == rvu = rvd
    | m == rvd = rvu
    | m == lvu = lvd
    | m == lvd = lvu
    | m == thr = thl
    | m == thl = thr
    | m == bhr = bhl
    | m == bhl = bhr
    | m == fc = fcc
    | m == fcc = fc
    | m == bc = bcc
    | m == bcc = bc
    | otherwise = m

cube_state_to_key :: Cube -> String
cube_state_to_key (Cube corners) = 
    corner_dissection Map.empty 1 "" (get_all_colors corners)

corner_dissection :: Map.Map Char Int -> Int -> String -> [Char] -> String
corner_dissection _ _ key [] = key
corner_dissection key_dict position key (c:cs) = 
    case Map.lookup c key_dict of 
        Just value -> corner_dissection key_dict position (show value ++ key) cs
        Nothing -> corner_dissection (Map.insert c position key_dict) (position + 1) (show position ++ key) cs
>>>>>>> Stashed changes
