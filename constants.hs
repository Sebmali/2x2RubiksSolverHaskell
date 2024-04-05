module Constants where 

import qualified Data.Map as Map 
import Data_Types

colors:: Colors
colors = "RGBYOW"
max_depth_limit :: Int 
max_depth_limit = 14
poss_corners :: [String]
poss_corners = ["Front-Right-Top", "Front-Left-Top", "Back-Left-Top", "Back-Right-Top", 
               "Front-Right-Bottom", "Front-Left-Bottom", "Back-Left-Bottom", "Back-Right-Bottom"]
poss_colors :: Colors
poss_colors = ['W', 'G', 'R', 'B', 'O', 'Y']
rvu, rvd, lvu, lvd, thr, thl, bhr, bhl, fc, fcc, bc, bcc :: Move
rvu = "right_vertical_up"
rvd = "right_vertical_down"
lvu = "left_vertical_up"
lvd = "left_vertical_down"
thr = "top_horizontal_right"
thl = "top_horizontal_left"
bhr = "bottom_horizontal_right"
bhl = "bottom_horizontal_left"
fc = "front_clockwise"
fcc = "front_counter_clockwise"
bc = "back_clockwise"
bcc = "back_counter_clockwise"

solved_cube :: [String]
solved_cube = ["R W B", "R W G", "O W G","O W B","R Y B","R Y G","O Y G","O Y B"]


-- All possible moves
all_moves, rvu_moves, rvd_moves, lvu_moves, lvd_moves, thr_moves, thl_moves :: [String]
bhr_moves, bhl_moves, fc_moves, fcc_moves, bc_moves, bcc_moves :: [String]
all_moves = [rvu, rvd, lvu, lvd, thr, thl, bhr, bhl, fc, fcc, bc, bcc]
rvu_moves = [rvu, lvd, thr, thl, bhr, bhl, fc, fcc, bc, bcc]
rvd_moves = [rvd, lvu, thr, thl, bhr, bhl, fc, fcc, bc, bcc]
lvu_moves = [rvd, lvu, thr, thl, bhr, bhl, fc, fcc, bc, bcc]
lvd_moves = [rvu, lvd, thr, thl, bhr, bhl, fc, fcc, bc, bcc]
thr_moves = [rvu, rvd, lvu, lvd, thr, bhl, fc, fcc, bc, bcc]
thl_moves = [rvu, rvd, lvu, lvd, thl, bhr, fc, fcc, bc, bcc]
bhr_moves = [rvu, rvd, lvu, lvd, thl, bhr, fc, fcc, bc, bcc]
bhl_moves = [rvu, rvd, lvu, lvd, thr, bhl, fc, fcc, bc, bcc]
fc_moves = [rvu, rvd, lvu, lvd, thr, thl, bhr, bhl, fc, bcc]
fcc_moves = [rvu, rvd, lvu, lvd, thr, thl, bhr, bhl, fcc, bc]
bc_moves = [rvu, rvd, lvu, lvd, thr, thl, bhr, bhl, fcc, bc]
bcc_moves = [rvu, rvd, lvu, lvd, thr, thl, bhr, bhl, fc, bcc]

--Corner indices for each move
rvu_ind, rvd_ind, lvu_ind, lvd_ind, thr_ind, thl_ind, bhr_ind, bhl_ind, fc_ind, fcc_ind, bc_ind, bcc_ind :: [Int]
rvu_ind = [0, 4, 7, 3]
rvd_ind = [0, 3, 7, 4]
lvu_ind = [1, 5, 6, 2]
lvd_ind = [1, 2, 6, 5]
thr_ind = [0, 1, 2, 3]
thl_ind = [0, 3, 2, 1]
bhr_ind = [4, 5, 6, 7]
bhl_ind = [4, 7, 6, 5]
fc_ind = [0, 1, 5, 4]
fcc_ind = [0, 4, 5, 1]
bc_ind = [2, 6, 7, 3]
bcc_ind = [2, 3, 7, 6]

--Color Indeces 1,2,3 in a corner
c1, c2, c3 :: Int
c1 = 0
c2 = 1
c3 = 2

--Face indices for solving check
front, back, top, bottom, right, left :: [Int]
front = [0, 1, 4, 5]
back = [2, 3, 6, 7]
top = [0, 1, 2, 3]
bottom = [4, 5, 6, 7]
right = [0, 3, 4, 7]
left = [1, 2, 5, 6]

--All possible corner orientations to check for correct initial cube
initial_possibilities :: [Corner]
initial_possibilities = [Corner['W','O','B'], Corner['W','B','O'], Corner['O','W','B'], Corner['O','B','W'], Corner['B','W','O'], Corner['B','O','W'],
                         Corner['W','O','G'], Corner['W','G','O'], Corner['O','W','G'], Corner['O','G','W'], Corner['G','W','O'], Corner['G','O','W'],
                         Corner['W','R','G'], Corner['W','G','R'], Corner['R','W','G'], Corner['R','G','W'], Corner['G','W','R'], Corner['G','R','W'],
                         Corner['W','R','B'], Corner['W','B','R'], Corner['R','W','B'], Corner['R','B','W'], Corner['B','W','R'], Corner['B','R','W'],
                         Corner['Y','O','B'], Corner['Y','B','O'], Corner['O','Y','B'], Corner['O','B','Y'], Corner['B','Y','O'], Corner['B','O','Y'],
                         Corner['Y','O','G'], Corner['Y','G','O'], Corner['O','Y','G'], Corner['O','G','Y'], Corner['G','Y','O'], Corner['G','O','Y'],
                         Corner['Y','R','G'], Corner['Y','G','R'], Corner['R','Y','G'], Corner['R','G','Y'], Corner['G','Y','R'], Corner['G','R','Y'],
                         Corner['Y','R','B'], Corner['Y','B','R'], Corner['R','Y','B'], Corner['R','B','Y'], Corner['B','Y','R'], Corner['B','R','Y']]

--All the possible pruned moves that are possible for a given move.
pruned_move_map :: Map.Map Move Moves
pruned_move_map = Map.fromList [
    (rvu, rvu_moves),
    (rvd, rvd_moves),
    (lvu, lvu_moves),
    (lvd, lvd_moves),
    (thr, thr_moves),
    (thl, thl_moves),
    (bhr, bhr_moves),
    (bhl, bhl_moves),
    (fc, fc_moves),
    (fcc, fcc_moves),
    (bc, bc_moves),
    (bcc, bcc_moves)
    ]

--Map of all conversions for a given move, i.e., the opposite move. (RVU -> RVD)
convert_move_map :: Map.Map Move Move
convert_move_map = Map.fromList [
    (rvu, rvd),
    (rvd, rvu),
    (lvu, lvd),
    (lvd, lvu),
    (thr, thl),
    (thl, thr),
    (bhr, bhl),
    (bhl, bhr),
    (fc, fcc),
    (fcc, fc),
    (bc, bcc),
    (bcc, bc)
    ]
