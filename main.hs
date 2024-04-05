{-
main.hs
Purpose: This file initiates the solving of the Rubiks Cube by setting up a initial cube
and a final cube for the purpose of bidirectional search algorithm. The input from the 
user must be in the format of a text file input.
Authors: Sebastian Maliczewski, Shayne Prakash
-}
import Corner 
import Cube
import System.IO
import Constants
import qualified Data.Set as Set 
import System.CPUTime
import Text.Printf
import Control.Exception

main :: IO ()
main = do 
   get_file >>= \x -> if x then return() else main

get_file :: IO Bool
get_file = do 
    putStr "Enter the file name: "
    file_name <- getLine
    file_result <- try (openFile file_name ReadMode) :: IO (Either IOException Handle)
    case file_result of 
        Left err -> do 
            putStrLn $ "Could not open file: " ++ show err 
            return False 
        Right file_handle -> do 
            contents <- hGetContents file_handle 
            let corners = lines contents
            print corners 
            let resultCube = create_corners corners empty_cube
            let finalCube = create_corners solved_cube empty_cube
            hClose file_handle
            if check_initial_cube resultCube then do
                start <- getCPUTime
                case solve_outer_cube resultCube finalCube of 
                    [] -> putStr "No solution found.\n" >> return False
                    path -> do 
                        putStr "Solution found: "
                        print path
                        end <- getCPUTime
                        let diff = (fromIntegral (end - start)) / (10^12)
                        printf "Computation time: %0.3f sec\n" (diff :: Double)
                        return True
            else putStr "Invalid Cube Configuration. Please try again.\n" >> return False

create_corners :: [String] -> Cube -> Cube
create_corners corner_string cube = create_corners_helper (reverse corner_string) cube

create_corners_helper :: [String] -> Cube -> Cube
create_corners_helper [] cube = cube
create_corners_helper (x:xs) cube = do 
    let colors = words x 
    let corner = new_corner colors 
    if is_valid_corner corner then create_corners_helper xs (add_corner corner cube)
    else error "Invalid Cube Configuration. Please try again.\n"
