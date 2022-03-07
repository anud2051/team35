module Play(main,play,wtfBoard) where
import BoardStatus
import MovePiece
import PossibleMoves
import ValidMove
import Data 
import Data.Char(digitToInt)

wtfBoard = [(Empty (1,1)), (Empty (1,2)), (Empty (1,3)), Piece King (1,4) White, (Empty (1,5)), (Empty (1,6)), (Empty (1,7)), (Empty (1,8)),(Empty (2,1)), (Empty (2,2)), (Empty (2,3)), (Empty (2,4)), (Empty (2,5)), (Empty (2,6)), (Empty (2,7)), (Empty (2,8)),(Empty (3,1)), (Empty (3,2)), (Empty (3,3)), (Empty (3,4)), (Empty (3,5)), (Empty (3,6)), (Empty (3,7)), (Empty (3,8)),(Empty (4,1)), (Empty (4,2)), (Empty (4,3)), (Empty (4,4)), (Empty (4,5)), (Empty (4,6)), (Empty (4,7)), (Empty (4,8)),(Empty (5,1)), (Empty (5,2)), (Empty (5,3)), (Empty (5,4)), (Empty (5,5)), (Empty (5,6)), (Empty (5,7)), (Empty (5,8)),(Empty (6,1)), (Empty (6,2)), (Empty (6,3)), (Empty (6,4)), (Empty (6,5)), (Empty (6,6)), (Empty (6,7)), (Empty (6,8)),(Empty (7,1)), (Empty (7,2)), (Empty (7,3)), Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black, (Empty (7,7)), (Empty (7,8)),(Empty (8,1)), (Empty (8,2)), (Empty (8,3)), (Empty (8,4)), Piece King (8,5) Black, (Empty (8,6)), (Empty (8,7)), Piece Queen (8,8) White]




{- main 
 main is called to start a game of chess. Used to give our play function the correct starting value, the initialBoard and the color White
-}
main :: IO ()
main = play (initialBoard, White)

{-play (gameState, color1)
  play is the gameloop which will continued until the game is over.
  SIDE EFFECTS: prints diffrent statements to the terminal depening on the players input. If the game continue, the function takes two inputs from a user and if the input is valid the gameState will be updated and and color1 will switch color. Then the function will call itself with the new argumnets.
-}
play :: (Board, PColor) -> IO ()
play (gameState, color1) = do
    showBoard gameState 8
    if isCheckMate (Piece King (whereIsKing gameState color1) color1) gameState then 
      putStrLn $ "Player " ++ show (oppositeColor color1) ++ " has won the game"
    else if isStaleMate gameState (Piece King (whereIsKing gameState color1) color1) then
      putStrLn $ "Stalemate! It's a draw!"
      else do
        putStrLn $ "Which " ++ show color1 ++ " piece do you want to move?"
        piece <- getLine 
        print "Where do you want to move it?"
        move <- getLine
        if correctInput piece && correctInput move then 
          if isSquareNotEmpty (inputToPosition piece) gameState then
            let piece1 = inputToPosition piece
                move1 = inputToPosition move
                pieceColor = posToColor (posToSquare piece1 gameState)
                validMove = isValidMove gameState (posToSquare piece1 gameState) move1
                whereisKing = whereIsKing (movePiece (posToSquare piece1 gameState) move1 gameState) color1
                notCheck = not (isCheck (Piece King whereisKing color1)((movePiece (posToSquare piece1 gameState) move1 gameState))) in
            if color1 == pieceColor && validMove && notCheck
            then 
              if isQueening (posToSquare piece1 gameState) move1
              then do
                newPiece <- choosePiece (move1, color1)
                play $ (replaceWithEmpty (posToSquare piece1 gameState) (movePiece newPiece move1 gameState), oppositeColor color1) 
              else play $ (movePiece (posToSquare piece1 gameState) move1 gameState, oppositeColor color1)
            else do
              print "Incorrect move" 
              play (gameState, color1)
          else do
            putStrLn "You selected an empty square"
            putStrLn "Please select a piece!"
            play (gameState,color1)
        else do
            putStrLn "Ivalid format!"
            putStrLn "examples of correct format: A4,H7,C5 etc "
            play (gameState,color1)



{- inputToPosition input
  converts a correct input to cordinates
  RETURNS: a tuple with the cordinates of input
  EXAMPLES: inputToPosition "A4" == (4,1)
            inputToPosition "F7" == (7,6)
-}
inputToPosition:: String -> Position 
inputToPosition [] = (0,0)
inputToPosition (s:s1:[])
  | s == 'A' = (digitToInt s1,1)
  | s == 'B' = (digitToInt s1,2)
  | s == 'C' = (digitToInt s1,3)
  | s == 'D' = (digitToInt s1,4)
  | s == 'E' = (digitToInt s1,5)
  | s == 'F' = (digitToInt s1,6)
  | s == 'G' = (digitToInt s1,7)
  | s == 'H' = (digitToInt s1,8)
  | otherwise = (read (show s1) :: Int,0)
inputToPosition (x:xs) = (0,0)


{- correctInput input
  checks if the input is valid
  RETURNS: True if input is valid, else False
  EXAMPLES: correctInput "B3" == True
            correctInput "G8" == False
-}
correctInput :: String -> Bool
correctInput s 
  | s == "A1" = True 
  | s == "A2" = True
  | s == "A3" = True
  | s == "A4" = True
  | s == "A5" = True
  | s == "A6" = True
  | s == "A7" = True
  | s == "A8" = True
  | s == "B1" = True
  | s == "B2" = True
  | s == "B3" = True
  | s == "B4" = True
  | s == "B5" = True
  | s == "B6" = True
  | s == "B7" = True
  | s == "B8" = True
  | s == "C1" = True
  | s == "C2" = True
  | s == "C3" = True
  | s == "C4" = True
  | s == "C5" = True
  | s == "C6" = True
  | s == "C7" = True
  | s == "C8" = True
  | s == "D1" = True
  | s == "D2" = True
  | s == "D3" = True
  | s == "D4" = True
  | s == "D5" = True
  | s == "D6" = True
  | s == "D7" = True
  | s == "D8" = True
  | s == "E1" = True
  | s == "E2" = True
  | s == "E3" = True
  | s == "E4" = True
  | s == "E5" = True
  | s == "E6" = True
  | s == "E7" = True
  | s == "E8" = True
  | s == "F1" = True
  | s == "F2" = True
  | s == "F3" = True
  | s == "F4" = True
  | s == "F5" = True
  | s == "F6" = True
  | s == "F7" = True
  | s == "F8" = True
  | s == "G1" = True
  | s == "G2" = True
  | s == "G3" = True
  | s == "G4" = True
  | s == "G5" = True
  | s == "G6" = True
  | s == "G7" = True
  | s == "G8" = True
  | s == "H1" = True
  | s == "H2" = True
  | s == "H3" = True
  | s == "H4" = True
  | s == "H5" = True
  | s == "H6" = True
  | s == "H7" = True
  | s == "H8" = True
  | otherwise = False



{- showBoardAux board row
  finds all squares at a specific row
  RETURNS: a list with all the char for respective Square on the row at the given board
  EXAMPLES: showBoardAux initialBoard 1 == "RNBKQBNR"
            showBoardAux initialBoard 4 == "        "
-}  
showBoardAux :: Board -> Int -> [Char]
showBoardAux [] _ = []
showBoardAux (piece@(Piece piecetype (x, y) color):xs) row 
  | x == row = fst(pieceToChar piece) : showBoardAux xs row
  | otherwise = showBoardAux xs row
showBoardAux (square@(Empty (x, y)):xs) row 
  | x == row = fst (pieceToChar square): showBoardAux xs row
  | otherwise = showBoardAux xs row
-- VARIANT: length of board
{- showBoard board n
  prints the current board to the terminal
  SIDE EFFECTS: prints the current board to the terminal
-}

showBoard :: [Square] -> Int -> IO ()
showBoard board n = do
  if n == 8 then do
    putStrLn ""
    putStrLn "   A     B     C     D     E     F     G     H"
    putStrLn "_________________________________________________"
    let xs = showBoardAux board n 
    putStrLn ("| "++ show (xs!!0) ++ " | " ++show (xs!!1) ++ " | "++show (xs!!2) ++ " | "++show (xs!!3) ++ " | "++show (xs!!4) ++ " | "++show (xs!!5) ++ " | "++show (xs!!6) ++ " | "++show (xs!!7) ++ " | " ++ show n )
    showBoard board (n-1)
  else do 
    if n > 0 then do
      let xs = showBoardAux board n 
      putStrLn ("| "++ show (xs!!0) ++ " | " ++show (xs!!1) ++ " | "++show (xs!!2) ++ " | "++show (xs!!3) ++ " | "++show (xs!!4) ++ " | "++show (xs!!5) ++ " | "++show (xs!!6) ++ " | "++show (xs!!7) ++ " | " ++ show n )
      showBoard board (n-1)
    else do
      putStrLn "-------------------------------------------------"
      putStrLn ""
      return ()


{-pieceToChar square
  convert all different types of squares to a more readible form
  RETURNS: A tuple containing a char which represent the piecetype along with the position of the given square. '_' represents a Empty square, low case characters represent White pieces and upper-cased characters represent Black pieces.
  EXAMPLES: pieceToChar (Piece Rock (2, 1) White) == ('R',(2,1))
            pieceToChar (Empty (4, 4)) == (' ',(4,4))
            pieceToChar (Piece King (8, 4) Black) == ('k',(8,4))
-}
pieceToChar :: Square -> (Char, Position)
pieceToChar (Empty (x, y)) = (' ', (x, y))
pieceToChar (Piece Rock (x, y) White) = ('R',(x, y))
pieceToChar (Piece Knight (x, y) White) = ('N',(x, y))
pieceToChar (Piece Bishop (x, y) White) = ('B',(x, y))
pieceToChar (Piece King (x, y) White) = ('K',(x, y))
pieceToChar (Piece Queen (x, y) White) = ('Q',(x, y))
pieceToChar (Piece Pawn (x, y) White) = ('P',(x, y))
pieceToChar (Piece Rock (x, y) Black) = ('r',(x, y))
pieceToChar (Piece Knight (x, y) Black) = ('n',(x, y))
pieceToChar (Piece Bishop (x, y) Black) = ('b',(x, y))
pieceToChar (Piece King (x, y) Black) = ('k',(x, y))
pieceToChar (Piece Queen (x, y) Black) = ('q',(x, y))
pieceToChar (Piece Pawn (x, y) Black) = ('p',(x, y))


{-isSquareNotEmpty pos board 
  checks if a square at position pos at the board is not empty
  RETURNS: True if the square with position pos is not empty at the given board, else False
  EXAMPLES: isSquareNotEmpty (4,4) initialBoard == False
            isSquareNotEmpty (1,5) initialBoard == True
-}
isSquareNotEmpty :: Position -> Board -> Bool
isSquareNotEmpty pos board 
  | posToSquare pos board == Empty pos = False 
  | otherwise = True 


{-posToColor piece
  extracts the color of a given piece
  PRE: piece == (Piece ptype pos color)
  RETURNS: the color (Black or White) of the given piece
  EXAMPLES: posToColor (Piece King (8,4) Black) == Black
            posToColor (Piece Pawn (2,2) White) == White
-}
posToColor :: Square -> PColor
posToColor (Piece ptype pos color) = color


