{-
data_types.hs
Purpose: This file contains all the data types and type synonyms used in the program.
This allows for easy access to all these types within the various modules.
-}
module Data_Types where

import qualified Data.Map as Map 

data Cube = Cube [Corner] deriving (Show)
data Corner = Corner [Char] deriving (Show, Eq)
type Result = (Bool, [String], Map.Map String Int, String)
type Depth = Int 
type Solution = [String]
type Path = [String]
type Memo = Map.Map String Int
type Move = String
type Moves = [String]
type Key = String 
type Color = Char
type Colors = [Char]