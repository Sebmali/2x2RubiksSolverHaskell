poss_corners :: [String]
poss_corners = ["Front-Right-Top", "Front-Left-Top", "Back-Left-Top", "Back-Right-Top", 
               "Front-Right-Bottom", "Front-Left-Bottom", "Back-Left-Bottom", "Back-Right-Bottom"]
poss_colors :: [Char]
poss_colors = ['W', 'G', 'R', 'B', 'O', 'Y']
rvu :: String
rvu = "right_vertical_up"
rvd :: String
rvd = "right_vertical_down"
lvu :: String
lvu = "left_vertical_up"
lvd :: String
lvd = "left_vertical_down"
thr :: String
thr = "top_horizontal_right"
thl :: String
thl = "top_horizontal_left"
bhr :: String
bhr = "bottom_horizontal_right"
bhl :: String
bhl = "bottom_horizontal_left"
fc :: String
fc = "front_clockwise"
fcc :: String
fcc = "front_counter_clockwise"
bc :: String
bc = "back_clockwise"
bcc :: String
bcc = "back_counter_clockwise"

-- All possible moves
all_moves :: [String]
all_moves = [rvu, rvd, lvu, lvd, thr, thl, bhr, bhl, fc, fcc, bc, bcc]
rvu_moves :: [String]
rvu_moves = [rvu, lvd, thr, thl, bhr, bhl, fc, fcc, bc, bcc]
rvd_moves :: [String]
rvd_moves = [rvd, lvu, thr, thl, bhr, bhl, fc, fcc, bc, bcc]
lvu_moves :: [String]
lvu_moves = [rvd, lvu, thr, thl, bhr, bhl, fc, fcc, bc, bcc]
lvd_moves :: [String]
lvd_moves = [rvu, lvd, thr, thl, bhr, bhl, fc, fcc, bc, bcc]
thr_moves :: [String]
thr_moves = [rvu, rvd, lvu, lvd, thr, bhl, fc, fcc, bc, bcc]
thl_moves :: [String]
thl_moves = [rvu, rvd, lvu, lvd, thl, bhr, fc, fcc, bc, bcc]
bhr_moves :: [String]
bhr_moves = [rvu, rvd, lvu, lvd, thl, bhr, fc, fcc, bc, bcc]
bhl_moves :: [String]
bhl_moves = [rvu, rvd, lvu, lvd, thr, bhl, fc, fcc, bc, bcc]
fc_moves :: [String]
fc_moves = [rvu, rvd, lvu, lvd, thr, thl, bhr, bhl, fc, bcc]
fcc_moves :: [String]
fcc_moves = [rvu, rvd, lvu, lvd, thr, thl, bhr, bhl, fcc, bc]
bc_moves :: [String]
bc_moves = [rvu, rvd, lvu, lvd, thr, thl, bhr, bhl, fcc, bc]
bcc_moves :: [String]
bcc_moves = [rvu, rvd, lvu, lvd, thr, thl, bhr, bhl, fc, bcc]

--Corner indices for each move
rvu_ind :: [Int]
rvu_ind = [0, 4, 7, 3]
rvd_ind :: [Int]
rvd_ind = [0, 3, 7, 4]
lvu_ind :: [Int]
lvu_ind = [1, 5, 6, 2]
lvd_ind :: [Int]
lvd_ind = [1, 2, 6, 5]
thr_ind :: [Int]
thr_ind = [0, 1, 2, 3]
thl_ind :: [Int]
thl_ind = [0, 3, 2, 1]
bhr_ind :: [Int]
bhr_ind = [4, 5, 6, 7]
bhl_ind :: [Int]
bhl_ind = [4, 7, 6, 5]
fc_ind :: [Int]
fc_ind = [0, 1, 5, 4]
fcc_ind :: [Int]
fcc_ind = [0, 4, 5, 1]
bc_ind :: [Int]
bc_ind = [2, 6, 7, 3]
bcc_ind :: [Int]
bcc_ind = [2, 3, 7, 6]

--Color Indeces 1,2,3 in a corner
c1 :: Int
c1 = 0
c2 :: Int
c2 = 1
c3 :: Int
c3 = 2

--Face indices for solving check
front :: [Int]
front = [0, 1, 4, 5]
back :: [Int]
back = [2, 3, 6, 7]
top :: [Int]
top = [0, 1, 2, 3]
bottom :: [Int]
bottom = [4, 5, 6, 7]
right :: [Int]
right = [0, 3, 4, 7]
left :: [Int]
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
