{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
import Corner 
import Cube
import System.IO
import Data.Type.Equality (apply)
import Constants

main :: IO ()
main = do 
    putStr "Would you like to input a file? (Y/N)\n"
    input <- getLine 
    if input == "Y" || input == "y" then get_file >>= \x -> if x then return() else main
    else if input == "N" || input == "n" then get_corners >>= \x -> if x then return() else main
    else do putStr "Invalid input. Please try again.\n"
            main

get_file :: IO Bool
get_file = do 
    putStr "Enter the file name: "
    input <- getLine
    fileHandle <- openFile input ReadMode
    contents <- hGetContents fileHandle 
    let corners = lines contents
    print corners 
    let resultCube = create_corners corners empty_cube
    let finalCube = create_corners solved_cube empty_cube -- for bidirectional search
    print resultCube
    print finalCube -- for bidirectional search
    let cube_key = cube_state_to_key resultCube
    print cube_key
    hClose fileHandle
    if check_initial_cube resultCube then 
        case solve_outer_cube resultCube of 
            [] -> do putStr "No solution found.\n"
                     return False
            path -> do putStr "Solution found: "
                       print path
                       return True
    else do putStr "Invalid Cube Configuration. Please try again.\n"
            return False

get_corners :: IO Bool
get_corners = do 
    putStr "Enter the colors of each corner on the cube.\n"
    putStr "Use W for White, G for Green, R for Red, B for Blue, O for Orange, and Y for Yellow.\n"
    putStr "Ensure to keep mind of the front of the cube at all points.\n"
    putStr "Enter the colors in the format: Color1 Color2 Color3\n"
    let corners = []
    inputs <- get_input 0 []  
    let resultCube = create_corners inputs empty_cube
    print resultCube
    if check_initial_cube resultCube then return True 
    else do putStr "Invalid Cube Configuration. Please try again.\n"
            return False 

get_input :: Int -> [String] -> IO [String] 
get_input i  xs | i == 8 = return xs
                | otherwise = do 
                    putStr "Enter the colors of the corner: "
                    input <- getLine
                    --let colors = lines input
                    let newXs = xs ++ [colors]
                    print newXs
                    get_input (i + 1) newXs

create_corners :: [String] -> Cube -> Cube
create_corners corner_string cube = create_corners_helper (reverse corner_string) cube

create_corners_helper :: [String] -> Cube -> Cube
create_corners_helper [] cube = cube
create_corners_helper (x:xs) cube = do 
    let colors = words x 
    let corner = new_corner colors 
    if is_valid corner then create_corners_helper xs (add_corner corner cube)
    else error "Invalid Cube Configuration. Please try again.\n"