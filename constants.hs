module Constants where 

max_depth_limit :: Int 
max_depth_limit = 14
poss_corners :: [String]
poss_corners = ["Front-Right-Top", "Front-Left-Top", "Back-Left-Top", "Back-Right-Top", 
               "Front-Right-Bottom", "Front-Left-Bottom", "Back-Left-Bottom", "Back-Right-Bottom"]
poss_colors :: [Char]
poss_colors = ['W', 'G', 'R', 'B', 'O', 'Y']
rvu, rvd, lvu, lvd, thr, thl, bhr, bhl, fc, fcc, bc, bcc :: String
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

<<<<<<< Updated upstream
=======
solved_cube :: [String]
solved_cube = ["R W B", "R W G", "O W G","O W B","R Y B","R Y G","O Y G","O Y B"]

>>>>>>> Stashed changes
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
initial_possibilities :: [[Char]]
initial_possibilities = [['W','O','B'], ['W','B','O'], ['O','W','B'], ['O','B','W'], ['B','W','O'], ['B','O','W'],
                         ['W','O','G'], ['W','G','O'], ['O','W','G'], ['O','G','W'], ['G','W','O'], ['G','O','W'],
                         ['W','R','G'], ['W','G','R'], ['R','W','G'], ['R','G','W'], ['G','W','R'], ['G','R','W'],
                         ['W','R','B'], ['W','B','R'], ['R','W','B'], ['R','B','W'], ['B','W','R'], ['B','R','W'],
                         ['Y','O','B'], ['Y','B','O'], ['O','Y','B'], ['O','B','Y'], ['B','Y','O'], ['B','O','Y'],
                         ['Y','O','G'], ['Y','G','O'], ['O','Y','G'], ['O','G','Y'], ['G','Y','O'], ['G','O','Y'],
                         ['Y','R','G'], ['Y','G','R'], ['R','Y','G'], ['R','G','Y'], ['G','Y','R'], ['G','R','Y'],
                         ['Y','R','B'], ['Y','B','R'], ['R','Y','B'], ['R','B','Y'], ['B','Y','R'], ['B','R','Y']]

