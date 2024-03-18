{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use list literal" #-}

module Cube
(
    Cube,
    empty_cube,
    check_cube,
    add_corner,
    solve_outer_cube,
    apply_move
) where

import Corner
import Corner (Corner, swap1)
import Constants

empty_cube :: Cube
add_corner :: Corner -> Cube -> Cube
check_cube :: Cube -> Bool
solve_outer_cube :: Cube -> Int -> String 
solve_cube :: Cube -> Int -> String
apply_move :: Cube -> String -> Cube
is_solved :: Cube -> Bool


data Cube = Cube [Corner]
    deriving (Show)

empty_cube = Cube []
add_corner corner (Cube corners) = Cube (corner:corners)

check_cube (Cube corners) = length corners == 8
-- make move take a string to rep the specific move, based on the move(maybe use case) we send the specific corners
-- to swap1

move :: Cube -> Int -> Int -> [Int] -> Cube
move (Cube corners) a b indices_to_swap =
    let temp = swap1 a b (corners !! (indices_to_swap !! 0))
        replace idx new_corner cube_state = take idx cube_state ++ [new_corner] ++ drop (idx + 1) cube_state
        cube1 = replace (indices_to_swap !! 0) (swap1 a b (corners !! (indices_to_swap !! 1))) corners
        cube2 = replace (indices_to_swap !! 1) (swap1 a b (cube1 !! (indices_to_swap !! 2))) cube1
        cube3 = replace (indices_to_swap !! 2) (swap1 a b (cube2 !! (indices_to_swap !! 3))) cube2
        final_cube = replace (indices_to_swap !! 3) temp cube3
    in Cube final_cube


solve_outer_cube cube x | x == 14 = ""
                        | otherwise = do solve_cube cube x
                                         solve_outer_cube cube (x + 1)

solve_cube cube x | x > 14 = "" --Solve the inside of the cube. 

is_solved (Cube corners) = 

    
apply_move (Cube corners) m = 
    case m of
        "RVU" -> move (Cube corners) 0 1 rvu_ind
        "RVD" -> move (Cube corners) 0 1 rvd_ind
        "LVU" -> move (Cube corners) 0 1 lvu_ind
        "LVD" -> move (Cube corners) 0 1 lvd_ind
        "THR" -> move (Cube corners) 0 2 thr_ind
        "THL" -> move (Cube corners) 0 2 thl_ind
        "BHR" -> move (Cube corners) 0 2 bhr_ind
        "BHL" -> move (Cube corners) 0 2 bhl_ind
        "FC" -> move (Cube corners) 1 2 fc_ind
        "FCC" -> move (Cube corners) 1 2 fcc_ind
        "BC" -> move (Cube corners) 1 2 bc_ind
        "BCC" -> move (Cube corners) 1 2 bcc_ind
        _ -> Cube corners
