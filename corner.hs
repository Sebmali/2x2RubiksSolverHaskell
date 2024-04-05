{-
Corner.py
Purpose: This file contains the Corner class which is used to represent the corners
of the Rubiks Cube. This class is used to represent the colors of the corner as well. 
Authors: Sebastian Maliczewski, Shayne Prakash
-}
module Corner
(
    Corner,
    new_corner,
    swap_colors,
    is_valid_corner,
    get_corner_colors,
    get_all_colors
)
where

import Constants
import Data_Types

new_corner :: [String] -> Corner
is_valid_corner :: Corner -> Bool
swap_colors :: Int -> Int -> Corner -> Corner
get_corner_colors :: Corner -> Colors
get_all_colors :: [Corner] -> Colors
swap_in_list :: Int -> Int -> [a] -> [a]

new_corner (x:y:z:xs) = Corner [x !! 0, y !! 0, z !! 0]

is_valid_corner (Corner [a, b, c]) = (a `elem` colors) && (b `elem` colors) && (c `elem` colors)

get_corner_colors (Corner cs) = cs

get_all_colors corners = [char | Corner chars <- corners, char <- chars]

swap_colors i j (Corner cs) = Corner (swap_in_list i j cs)

swap_in_list i j xs = [if k == i then xs !! j 
    else if k == j then xs !! i 
    else xs !! k | k <- [0..length xs - 1]]