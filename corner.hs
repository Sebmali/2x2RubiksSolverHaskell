module Corner
(
    Corner,
    newCorner,
    colors,
    isValid

)
where

newCorner :: [Char] -> Corner
isValid :: Corner -> Bool
colors:: String

data Corner = Corner [Char]
    deriving (Show)


newCorner [a, b, c] = Corner [a, b, c]

colors = "RGBYOW"

isValid (Corner [a, b, c]) = (a `elem` colors) && (b `elem` colors) && (c `elem` colors)