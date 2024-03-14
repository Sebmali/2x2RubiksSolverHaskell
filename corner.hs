{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Corner
(
    Corner,
    new_corner,
    colors,
    swap1,
    is_valid

)
where
import Data.Tuple (swap)

new_corner :: String -> Corner
is_valid :: Corner -> Bool
swap1 :: Int -> Int -> Corner -> Corner
colors:: String

data Corner = Corner [Char]
    deriving (Show)


new_corner [a, b, c] = Corner [a, b, c]

colors = "RGBYOW"

is_valid (Corner [a, b, c]) = (a `elem` colors) && (b `elem` colors) && (c `elem` colors)

swap1 i j (Corner cs) = Corner (swap_in_list i j cs)

-- what if we give swap_in_list the entire cube and also give it the specific corner to swap(int representation)
-- in addition to the two indices to swap
swap_in_list :: Int -> Int -> [a] -> [a]
swap_in_list i j xs = [if k == i then xs !! j else if k == j then xs !! i else xs !! k | k <- [0..length xs - 1]]
