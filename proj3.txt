     CS 3360 - Design and Implementation of Programming Languages

	    PROJECT 3: FUNCTIONAL PROGRAMMING WITH HASKELL
		 (File $Date: 2020/04/25 02:04:53 $)

Due: April 30, 2020

This assignment may be done in pairs. If you work in pairs, however,
you need to fill out the contribution form (see the course website).

The purpose of this assignment is to understand the concepts of
functional programming and have a taste of it in Haskell [1].

In this assignment, you are to to write a Haskell program for playing
omok games. Omok, meaning "five pieces", is a two-player strategy game
typically played with go pieces, black and white stones, on a go board
with 15x15 intersections (or places) [Wikipedia]. It can also be
played with a paper and pencils because stones, once placed, are not
allowed to be moved or removed from the board. The two players
alternate in placing a stone of their color on an empty intersection,
and the goal of the game is to place one's stones in a row of five
consecutive intersections vertically, horizontally, or diagonally. The
winner is the first player to get an unbroken row of five stones.

As in the AspectJ project, you will use the model-view-control (MVC)
pattern. Your Haskell program must consist of two modules, one for the
game model (M) and the other for a console-based UI (VC). To write the
second module, you will need to learn about the basic I/O operations
in Haskell (refer to Haskell tutorials such as
https://www.haskell.org/tutorial/io.html). In Haskell, an I/O function
may call non-I/O functions, but a non-I/O function may not call I/O
functions. You shouldn't include any I/O function in the first module
(M).

Do not use any library function other than the standard Prelude
functions that are automatically imported into every Haskell module
(see Part II below for an exception).

Part I. (65 points; 101 lines of source code) Develop a Haskell module
named Board to model an omok board and two players. As said earlier,
the idea is to separate the model part of your program from the UI
part to be developed in Part II below. Thus, no UI (especially, I/O)
function should be defined in this module. The following functions are
suggested to be written in this module. They will be useful in writing
the UI module in Part II.

1. (8 points) Creating a board and accessing its elements

   mkBoard n
     Return an empty nxn board, where n is a positive number. A 1-based 
     pair of indices (x,y) will be used to access a specific place of
     the board, where x and y are column and row indices, respectively.
     However, it's up to you how to represent an omok board concretely. 
   
   mkPlayer
     Create and return the first player. It's up to you how to 
     represent an omok player concretely. E.g., mkPlayer = 1
  
   mkOpponent
     Create and return the second player, i.e., the opponent. E.g.,
     mkOpponent = 2

   size bd
     Return the size of a board bd, n for an nxn board.
   
   row y bd
     Return a row y of a board bd, where y is a 1-based index. It returns
     a list of size n, where n is the size of bd.

   column x bd
     Return a column x of a board bd, where x is a 1-based index. It
     returns a list of size n, where n is the size of bd.

2. (12 points) Checking places and placing stones

   mark x y bd p
     Mark a place (x,y) in a board bd by a player p, where x and y 
     are 1-based column and row indices. The specified place is assumed
     to be empty (see below).

   isEmpty x y bd
     Is a place (x,y) of a board bd unmarked or a stone not placed? 
     The x and y are 1-based column and row indices.     

   isMarked x y bd
     Does a place (x,y) of a board bd have a stone placed? The x and y 
     are 1-based column and row indices.     

   isMarkedBy x y bd p
     Does a place (x,y) of a board bd have a stone placed by a player p?
     The x and y are 1-based column and row indices.     

   marker x y board 
     Return the player of the stone placed on a place (x,y) of a board 
     bd. The x and y are 1-based column and row indices.

   isFull bd
     Are all places of board bd marked, i.e., there is no empty place?

3. (35 points) Determining the outcome

    isWonBy bd p
      Is the game played on a board bd won by a player p?

    isDraw bd
      Is the game played on a board bd ended in a draw?

    isGameOver bd
      Is the game played on a board bd over?

4. (10 points) Converting to a string for printing

    boardToStr playerToChar bd

      Return a string representation of a board bd. This is a
      higher-order function, and playerToChar is a function that
      converts a player to a character representation, e.g., 'O' and
      'X' (see Part II below). A formatted sample return value is
      shown below; it is produced with a playerToChar function that
      maps the first player to 'O', the second player to 'X', and
      others to '.'.

     " x 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5\n
      y ------------------------------\n
      1| . . . . . . . . . . . . . . .\n
      2| . . . . . . . . . . . . . . .\n
      3| . . . . . . . . . . . . . . .\n
      4| . . . . . . . . . . . . . . .\n
      5| . . . . . . . . . . . . . . .\n
      6| . . . . . . . . O . . . . . .\n
      7| . . . . . . . X X . . . . . .\n
      8| . . . . . . X O O . . . . . .\n
      9| . . . . . . . . . . . . . . .\n
      0| . . . . . . . . . . . . . . .\n
      1| . . . . . . . . . . . . . . .\n
      2| . . . . . . . . . . . . . . .\n
      3| . . . . . . . . . . . . . . .\n
      4| . . . . . . . . . . . . . . .\n
      5| . . . . . . . . . . . . . . ."

Part II. (35 points; 112 lines) Develop a Haskell module named Main
that provides a console-based UI for playing an omok game. For this
part, you may need to import System.IO and System.Random modules. Your
Main module should define all the I/O functions listed below.

1. (15 points) Reading user inputs and printing outputs.

   playerToChar p
     Return a character representation of a player p. It returns a Char
     value. This function may be used to print the current state of
     a board (see the boardToStr function in Part I).

   readXY bd p
     Read a 1-based pair of indices (x, y) for player p, denoting an 
     unmarked place in a board bd. The function reads inputs from the
     standard input (stdin) and returns an IO value such as IO(Int,Int)
     or IO(Integer,Integer).

     The following IO functions may be useful.

     putStr, putStrLn - print a string to the standard out
     getLine - read a line from the standard in
     reads:: [(Integer, String)] - parse an Integer from a string

     For example, the following IO function reads lines from stdin
     until a positive Integer is read.

     getX = do
       putStrLn "Enter a positive value?"
       line <- getLine
       let parsed = reads line :: [(Integer, String)] in
         if length parsed == 0
         then getX'
         else let (x, _) = head parsed in
           if x > 0 
           then return x
           else getX'
       where
         getX' = do
           putStrLn "Invalid input!"
           getX

2. (20 points) Playing game

   main
    Main function to play an omok game between two human players.
    It returns an IO() value. The dimension of the board is 15x15, 
    and user inputs are read from the standard input (see the readXY 
    function below) and outputs like the board state and the game
    outcome are printed on the standard output. 

Part III. (20+ bonus points) You can earn bonus points by implementing
the following features. However, your bonus points count only when you
complete all the previous functions for regular points.    

1. (5 points) Let a player be able to quit a game by entering a special
   value, say -1, for x or y coordinates in the readXY function above.

2. (15+ points) Support a play against a computer by implementing a
   computer move strategy. An easiest way to add this feature is to
   define a strategy function, say getRandomXY or getSmartXY, and 
   use it in place of the readXY function for the opponent. Use the
   System.Random module to generate a random value, e.g., 
   x <- randomRIO(1,15) to generate a random number between 1 and 15
   inclusive.

3. Other interesting features of your own, e.g., highlighting the
   winning row.

TESTING

   You code should run with the Hugs Haskell interpreter available
   from https://www.haskell.org/hugs/. That is, your code should be
   compilable with GHC 6.6 or higher. If you use the Haskell Platform
   available from https://www.haskell.org/platform/, do not use any
   ghci-specific features.

WHAT AND HOW TO TURN IN
   
   Submit your source code files along with any supporting documents
   through the Assignment Submission page found in the Homework
   section of the course website. The page will ask you to zip all
   your files and upload a single archive file. The zip archive file
   should include only a single directory named YourFirstNameLastName
   which contains all your source code files and other support files
   needed to run your program. You should turn in your code before
   class on the due date.

   If you work in pair, make only one submission through the
   Assignment Submission page by specifying both names during the
   submission; make sure to include the contribution form in your
   submission.

GRADING

   You will be graded, in part, on how clear your code is.
   Excessively long code will be penalized: don't repeat code in
   multiple places. Your code should be well documented and sensibly
   indented so it is easy to read.

   Be sure your name is in the comments in your code.

REFERENCES 

  [1] Alejandro Seraano Mena, Beginning Haskell, A Project-Based
      Approach, Apress, 2014. Free ebook through UTEP library.