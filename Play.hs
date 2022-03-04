module Play(main,play) where
import BoardStatus
import MovePiece
import PossibleMoves
import ValidMove
import Data 


--showBoard creates a visualization of the given board
showBoard board = do
  print(showBoardAux board 1)
  print(showBoardAux board 2)
  print(showBoardAux board 3)
  print(showBoardAux board 4)
  print(showBoardAux board 5)
  print(showBoardAux board 6)
  print(showBoardAux board 7)
  print(showBoardAux board 8)

{-pieceToChar square
  convert all different types of squares to a more readible form
  RETURNS: A tuple containing a char which represent the piecetype along with the position of the given square. '_' represents a Empty square, low case characters represent White pieces and upper-cased characters represent Black pieces.
  EXAMPLES: pieceToChar (Piece Rock (2, 1) White) == ('R',(2,1))
            pieceToChar (Empty (4, 4)) == (' ',(4,4))
            pieceToChar (Piece King (8, 4) Black) == ('k',(8,4))
-}
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

{-showBoardAux xs row
  returns a list of all squares at the given row inside the board xs
  PRE: 0 < row < 9 
  RETURNS: a list of squares which occurs at the given row in the   given board xs
  EXAMPLES:  showBoardAux initialBoard 2 == [('P',(2,1)),('P',(2,2)),('P',(2,3)),('P',(2,4)),('P',(2,5)),('P',(2,6)),('P',(2,7)),('P',(2,8))]
            showBoardAux initialBoard 3 == [(' ',(3,1)),(' ',(3,2)),(' ',(3,3)),(' ',(3,4)),(' ',(3,5)),(' ',(3,6)),(' ',(3,7)),(' ',(3,8))]

-}
--VARIANT: length of xs
showBoardAux :: Board -> Int -> [(Char,(Int,Int))]
showBoardAux [] _ = []
showBoardAux (piece@(Piece piecetype (x, y) color):xs) row 
  | x == row = pieceToChar piece: showBoardAux xs row
  | otherwise = showBoardAux xs row
showBoardAux (square@(Empty (x, y)):xs) int 
  | x == int = pieceToChar square: showBoardAux xs int
  | otherwise = showBoardAux xs int



-- Our main function which will be called to start a game of chess
main = play (initialBoard, White)
--play is the gameloop which will continued until the game is over
play (gameState, color1) = do
    showBoard gameState
    if isCheckMate (Piece King (whereIsKing gameState color1) color1) gameState then 
      putStrLn $ "Player " ++ show (oppositeColor color1) ++ " has won the game"
    --quit
    else if isStaleMate gameState (Piece King (whereIsKing gameState color1) color1) then
      putStrLn $ "Stalemate! It's a draw!"
      else do
        putStrLn $ "Which " ++ show color1 ++ " piece do you want to move?"
        piece <- getLine 
        print "Where do you want to move it?"
        move <- getLine
        if isInputAPosition piece && isInputAPosition move then 
          if isSquareNotEmpty (readInput piece) gameState then
            let piece1 = readInput piece
                move1 = readInput move
                pieceColor = posToColor (posToSquare piece1 gameState)
                validMove = isValidMove gameState (posToSquare piece1 gameState) move1
                whereisKing = whereIsKing (movePiece (posToSquare piece1 gameState) move1 gameState) color1
                notCheck = not (isCheck (Piece King whereisKing color1)((movePiece (posToSquare piece1 gameState) move1 gameState))) in
            if color1 == pieceColor && validMove && notCheck
            then 
              if isQueening (posToSquare piece1 gameState) move1
              then do
                newPiece <- choosePiece (move1, color1)
                play $ (movePiece newPiece move1 gameState, oppositeColor color1) 
              else play $ (movePiece (posToSquare piece1 gameState) move1 gameState, oppositeColor color1)
            else do
              print "Incorrect move" 
              play (gameState, color1)
          else do
            putStrLn "You selected an empty square"
            putStrLn "Please choose a piece!"
            play (gameState,color1)
        else do
            putStrLn "Ivalid format!"
            putStrLn "correct format: (x,y)"
            play (gameState,color1)

{-isSquareNotEmpty pos board 
  checks if a sqaure a position pos at the board is empty
  RETURNS: True if the square with position pos is empty, else False
  EXAMPLES: isSquareNotEmpty (4,4) initialBoard == True
            isSquareNotEmpty (1,5) initialBoard 
-}
isSquareNotEmpty pos board 
  | posToSquare pos board == Empty pos = False 
  | otherwise = True 


{-isInputAPosition s
  checks if the given input s is in the format of a position
  RETURNS: True if s is at the format of a Position, else False
  EXAMPLES: isInputAPosition "(1,1)" == True
            isInputAPosition "(4,2)" == True
            isInputAPosition "(9,9)" == False
            isInputAPosition "abc" == False
-}
isInputAPosition :: String -> Bool
isInputAPosition s 
  |s == "(1,1)" = True
  |s == "(1,2)" = True
  |s == "(1,3)" = True
  |s == "(1,4)" = True
  |s == "(1,5)" = True
  |s == "(1,6)" = True
  |s == "(1,7)" = True
  |s == "(1,8)" = True
  |s == "(2,1)" = True
  |s == "(2,2)" = True
  |s == "(2,3)" = True
  |s == "(2,4)" = True
  |s == "(2,5)" = True
  |s == "(2,6)" = True
  |s == "(2,7)" = True
  |s == "(2,8)" = True
  |s == "(3,1)" = True  
  |s == "(3,2)" = True
  |s == "(3,3)" = True
  |s == "(3,4)" = True
  |s == "(3,5)" = True
  |s == "(3,6)" = True
  |s == "(3,7)" = True
  |s == "(3,8)" = True
  |s == "(4,1)" = True
  |s == "(4,2)" = True
  |s == "(4,3)" = True
  |s == "(4,4)" = True
  |s == "(4,5)" = True
  |s == "(4,6)" = True
  |s == "(4,7)" = True
  |s == "(4,8)" = True
  |s == "(5,1)" = True
  |s == "(5,2)" = True
  |s == "(5,3)" = True
  |s == "(5,4)" = True
  |s == "(5,5)" = True
  |s == "(5,6)" = True
  |s == "(5,7)" = True
  |s == "(5,8)" = True
  |s == "(6,1)" = True
  |s == "(6,2)" = True
  |s == "(6,3)" = True
  |s == "(6,4)" = True
  |s == "(6,5)" = True
  |s == "(6,6)" = True
  |s == "(6,7)" = True
  |s == "(6,8)" = True
  |s == "(7,1)" = True
  |s == "(7,2)" = True
  |s == "(7,3)" = True
  |s == "(7,4)" = True
  |s == "(7,5)" = True
  |s == "(7,6)" = True
  |s == "(7,7)" = True
  |s == "(7,8)" = True
  |s == "(8,1)" = True
  |s == "(8,2)" = True
  |s == "(8,3)" = True
  |s == "(8,4)" = True
  |s == "(8,5)" = True
  |s == "(8,6)" = True
  |s == "(8,7)" = True
  |s == "(8,8)" = True
  |otherwise = False
{-readInput s
  converts a string s to a Position
  RETURNS: a the string but read as a postion
  EXAMPLES: readInput "(3,3)" == (3,3)
            readInput "(2,5)" == (2,5)
-}
readInput :: String -> Position
readInput s = read s :: Position
{-posToColor piece
  extracts the color of a given piece
  PRE: piece == (Piece ptype pos color)
  RETURNS: the color (Black or White) of the given piece
  EXAMPLES: posToColor (Piece King (8,4) Black) == Black
            posToColor (Piece Pawn (2,2) White) == White
-}
posToColor (Piece ptype pos color) = color


