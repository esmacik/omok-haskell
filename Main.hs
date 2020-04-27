-- Erik Macik
-- Main module for Omok game

module Main where
    import Board
    import System.IO
    import System.Random

    -- Return a character representation of a player p.
    playerToChar:: Int -> Char
    playerToChar p
        | p == 1 = '0'
        | p == 2 = 'X'
        | otherwise = '.'

    readXY:: [[Int]] -> Int -> IO(Int, Int)
    readXY bd p = do
        putStrLn ("Player " ++ show(p) ++ ", enter x and y coordinates separated by spaces (must be >= 1 and <= " ++ show(size bd) ++ "): ")
        line <- getLine
        let enteredValues = map read (words line)
        if length enteredValues /= 2 || enteredValues!!0 < 1 || enteredValues!!0 > (size bd) || enteredValues!!1 < 0 || enteredValues!!1 > (size bd) then do
            putStrLn "Invalid input!"
            readXY bd p
        else if isMarked (enteredValues!!0) (enteredValues!!1) bd then do
            putStrLn "Place is not empty!"
            readXY bd p
        else
            return (enteredValues!!0, enteredValues!!1)
    
    main :: IO ()
    main = do
        let bd = mkBoard 15
        putStrLn (boardToStr playerToChar bd)
        main' bd

    main':: [[Int]] -> IO()
    main' bd = do 
        playerCoordinates <- readXY bd 1
        let bdPlayerMove = mark (fst playerCoordinates) (snd playerCoordinates) bd 1
        putStrLn (boardToStr playerToChar bdPlayerMove)
        if isWonBy bdPlayerMove 1 then do
            putStrLn "Player 1 is the winner!"
            return ()
        else do
            opponentCoordinates <- readXY bdPlayerMove 2
            let bdOpponentMove = mark (fst opponentCoordinates) (snd opponentCoordinates) bdPlayerMove 2
            putStrLn (boardToStr playerToChar bdOpponentMove)
            if isWonBy bdOpponentMove 2 then do
                putStrLn "Player 2 is the winner!"
                return ()
            else if isDraw bdOpponentMove then do
                putStrLn "Game is a draw!"
                return ()
            else do
                main' bdOpponentMove
