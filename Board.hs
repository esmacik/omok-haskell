-- Erik Macik
-- Board module for Omok game

module Board where
    -- PART 1

    -- Function that creates an nxn board. Utilizes mkBoardHelper.
    mkBoard:: Int -> [[Int]]
    mkBoard 0 = [[]]
    mkBoard n = mkBoardHelper n n

    mkBoardHelper:: Int -> Int -> [[Int]]
    mkBoardHelper 1 n = [mkRow n]
    mkBoardHelper m n = mkRow n : mkBoardHelper (m - 1) n

    mkRow:: Int -> [Int]
    mkRow 0 = []
    mkRow n = 0 : mkRow (n-1)

    -- Create and return the first player
    mkPlayer:: Int
    mkPlayer = 1

    -- Create and return the second player
    mkOpponent:: Int
    mkOpponent = 2

    -- Return the size of a board bd, n for an nxn board
    size:: [[Int]] -> Int
    size bd = length bd

    -- Return a row y or a board bd, where y is a 1 based index
    row:: Int -> [[Int]] -> [Int]
    row y bd = bd !! (y - 1)

    -- Return a column x of a board bd, where x is a 1-based index
    column:: Int -> [[Int]] -> [Int]
    column x bd = columnHelper x bd (size(bd))

    -- columnHelper:: Int -> [[Int]] -> Int -> [Int]
    columnHelper x bd y
        | y == 1 = [row y bd !! (x-1)]
        | otherwise = columnHelper x bd (y-1) ++ [row (y-1) bd !! (x-1)]
    
    







    -- PART 2

    -- Mark a place (x,y) in a board bd by a player p, where x and y are 1-based column and row indicies
    mark:: Int -> Int -> [[Int]] -> Int -> [[Int]]
    mark x y (firstRow : rest) p
        | y == 1 = (markRow x firstRow p) : rest
        | otherwise = firstRow : mark x (y-1) rest p
    
    markRow:: Int -> [Int] -> Int -> [Int]
    markRow x (head : tail) p
        | x == 1 && head /= 0 = head:tail
        | x == 1 = p : tail
        | otherwise = head : markRow (x-1) tail p

    -- Is a place (x,y) of a board bd unmarked or a stone not places?
    isEmpty:: Int -> Int -> [[Int]] -> Bool
    isEmpty x y bd = place == 0
        where place = (row y bd) !! (x-1)

    -- Does a place (x,y) of a board bd have a stone placed?
    isMarked:: Int -> Int -> [[Int]] -> Bool
    isMarked x y bd = not (isEmpty x y bd)

    -- Does a place (x,y) of a board bd have a stone placed by a player p?
    isMarkedBy:: Int -> Int -> [[Int]] -> Int -> Bool
    isMarkedBy x y bd p
        | isEmpty x y bd = False
        | otherwise = (row y bd) !! (x-1) == p

    -- Retrun the player of the ston placed on a place (x,y) of a board bd
    marker:: Int -> Int -> [[Int]] -> Int
    marker x y bd = row y bd !! (x-1)

    -- Are all places of board bd marked?
    isFull:: [[Int]] -> Bool
    isFull [[]] = True
    isFull [row] = isRowFull row
    isFull (head : tail)
        | isRowFull head = isFull tail
        | otherwise = False

    isRowFull:: [Int] -> Bool
    isRowFull [] = True
    isRowFull (head : tail)
        | head == 0 = False
        | otherwise = isRowFull tail

    -- Return a string representation of a board bd.
    boardToStr:: (Int -> Char) -> [[Int]] -> String
    boardToStr f [] = "\n"
    boardToStr f (firstRow : rest) = rowToStr f firstRow ++ "\n" ++ boardToStr f rest

    rowToStr:: (Int -> Char) -> [Int] -> String
    rowToStr _ [] = ""
    rowToStr f (first: rest) = [f first] ++ " " ++ rowToStr f rest




    fakeBoard:: [[Int]]
    fakeBoard = [[1,0,0,0,0],[0,1,0,0,0],[0,0,1,0,0],[0,0,0,1,0],[0,0,0,0,1]]


    -- PART 3
    -- Is the game played on a board bd won by a player p?
    isWonBy:: [[Int]] -> Int -> Bool
    isWonBy bd p
        | elem True [ isWonByRow (row n bd) p 0 | n <- [1..(size bd)] ] = True
        | elem True [ isWonByRow (column n bd) p 0 | n <- [1..(size bd)] ] = True
        | elem True [ isWonByRow (row n (diagonals bd)) p 0 | n <- [1..(size(diagonals bd))] ] = True
        | elem True [ isWonByRow (row n (diagonals (reverse (bd)))) p 0 | n <- [1..(size(diagonals bd))] ] = True
        | otherwise = False

    -- -- Check if this straight line from the board contains a win
    isWonByRow:: [Int] -> Int -> Int -> Bool
    isWonByRow givenRow p numSeen
        | numSeen == 5 = True
        | givenRow == [] = False
        | givenRow !! 0 == p = isWonByRow (drop 1 givenRow) p (numSeen + 1)
        | otherwise = isWonByRow (drop 1 givenRow) p (numSeen)

    -- Is the game played on a board bd ended in a draw?
    isDraw:: [[Int]] -> Bool
    isDraw bd = isFull bd

    --Is the game played on a board bd over?
    isGameOver:: [[Int]] -> Bool
    isGameOver bd 
        | isWonBy bd mkPlayer = True
        | isWonBy bd mkOpponent = True
        | isDraw bd = True
        | otherwise = False

    
    diagonals [] = []
    diagonals ([]:xss) = xss
    diagonals xss = zipWith (++) (map ((:[]) . head) xss ++ repeat []) ([]:(diagonals (map tail xss)))
