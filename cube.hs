{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use list literal" #-}

module Cube
(
    Cube,
    empty_cube,
    add_corner,
    move



)
where
import Corner
import Corner (Corner, swap1)

empty_cube :: Cube
add_corner :: Corner -> Cube -> Cube
move :: Cube -> String -> Cube


data Cube = Cube [Corner]
    deriving (Show)

empty_cube = Cube []
add_corner corner (Cube corners) = Cube (corner:corners)

-- make move take a string to rep the specific move, based on the move(maybe use case) we send the specific corners
-- to swap1

move cube ints =
    let
        temp = corners !! (int !! 0)
        
    
apply_move (Cube corners) m = 
    case m of
        "RVU" -> Cube (swap1 0 1 (corners !! 4) : corners !! 1 : corners !! 2 : swap1 0 1 (corners !! 0) : swap1 0 1 (corners !! 7) : corners !! 5 : corners !! 6 : swap1 0 1 (corners !! 3) : [])
        "RVD" -> move cube [ints]
        "LVU" -> Cube (swap1 0 1 (corners !! 0) : swap1 1 2 (corners !! 1) : swap1 2 3 (corners !! 2) : swap1 3 0 (corners !! 3) : [corners !! 4, corners !! 5])
        "LVD" -> Cube (swap1 0 3 (corners !! 0) : swap1 3 2 (corners !! 3) : swap1 2 1 (corners !! 2) : swap1 1 0 (corners !! 1) : [corners !! 4, corners !! 5])
        "THR" -> Cube (swap1 0 1 (corners !! 0) : swap1 1 2 (corners !! 1) : swap1 2 3 (corners !! 2) : swap1 3 0 (corners !! 3) : [corners !! 4, corners !! 5])
        "THL" -> Cube (swap1 0 1 (corners !! 0) : swap1 1 2 (corners !! 1) : swap1 2 3 (corners !! 2) : swap1 3 0 (corners !! 3) : [corners !! 4, corners !! 5])
        "BHR" -> Cube (swap1 0 1 (corners !! 0) : swap1 1 2 (corners !! 1) : swap1 2 3 (corners !! 2) : swap1 3 0 (corners !! 3) : [corners !! 4, corners !! 5])
        "BHL" -> Cube (swap1 0 1 (corners !! 0) : swap1 1 2 (corners !! 1) : swap1 2 3 (corners !! 2) : swap1 3 0 (corners !! 3) : [corners !! 4, corners !! 5])
        "FC" -> Cube (swap1 0 1 (corners !! 0) : swap1 1 2 (corners !! 1) : swap1 2 3 (corners !! 2) : swap1 3 0 (corners !! 3) : [corners !! 4, corners !! 5])
        "FCC" -> Cube (swap1 0 1 (corners !! 0) : swap1 1 2 (corners !! 1) : swap1 2 3 (corners !! 2) : swap1 3 0 (corners !! 3) : [corners !! 4, corners !! 5])
        "BC" -> Cube (swap1 0 1 (corners !! 0) : swap1 1 2 (corners !! 1) : swap1 2 3 (corners !! 2) : swap1 3 0 (corners !! 3) : [corners !! 4, corners !! 5])
        "BCC" -> Cube (swap1 0 1 (corners !! 0) : swap1 1 2 (corners !! 1) : swap1 2 3 (corners !! 2) : swap1 3 0 (corners !! 3) : [corners !! 4, corners !! 5])
        _ -> Cube corners
