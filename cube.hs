module Cube
(
    Cube,
    emptyCube,
    addCorner

)
where

import Corner

emptyCube :: Cube
addCorner :: Corner -> Cube -> Cube

data Cube = Cube [Corner]
    deriving (Show)

emptyCube = Cube []
addCorner corner (Cube corners) = Cube (corner:corners)