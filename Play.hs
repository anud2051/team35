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

{-showBoardAux board row
    returns a list of all squares at the given row inside board
-}
showBoardAux :: Board -> Int -> Board
showBoardAux [] _ = []
showBoardAux (piece@(Piece piecetype (x, y) color):xs) row 
  | x == row = piece: showBoardAux xs row
  | otherwise = showBoardAux xs row
showBoardAux (square@(Empty (x, y)):xs) int 
  | x == int = square: showBoardAux xs int
  | otherwise = showBoardAux xs int



-- Our main function which will be called to start a game of chess

main = play (initialBoard, White)
--play is the gameloop which will continue until the game is over
play (gameState, color1) = do
    showBoard gameState
    putStrLn $ "Which " ++ show color1 ++ " piece do you want to move?"
    piece1 <- getLine 
    print "Where do you want to move it?"
    move1 <- getLine

    if color1 == stringToSquare piece1 && (validMove gameState (read piece1 :: Square) (read move1 :: Position)) && not (isCheck (Piece King (whereIsKing ((movePiece (read piece1::Square)) (read move1 :: Position) gameState) (Piece King (1,1) color1)) color1) ((movePiece (read piece1::Square) (read move1 :: Position) gameState)))
    then 
      if queening (read piece1 :: Square) (read move1 :: Position) 
      then do
      newPiece <- choosePiece ((read move1 :: Position), color1)
      play $ (movePiece newPiece (read move1 :: Position) gameState, oppositeColor color1) 
      else play $ (movePiece (read piece1::Square) (read move1 :: Position) gameState, oppositeColor color1)
    else do
    print "Incorrect move" 
    play (gameState, color1)

stringToSquare string = color
  where 
    Piece ptype pos color = read string :: Square

oppositeColor color
  | color == White = Black
  | otherwise = White
