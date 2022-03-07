-- correct [a,b]
-- | (a == 'A' || a == 'B' || a == 'C' || a == 'D' || a == 'E' || a == 'F' || a == 'G' || a == 'H') 
--   && (b == 1 || b == 2 ||b == 3 || b == 4 || b == 5 || b == 6 || b == 7 || b == 8 ||) == True
-- | otherwise == False
-- correctInput _ = False


-- pieceToChar??? just 'r' intead of ('r', (x,y))


-- add directions in possible moves



{-the order inside the list is as follows:
      element1 = closest piece in the North direction
      element2 = closest piece in the South direction
      element3 = closest piece in the West direction
      element4 = closest piece in the East direction
      element5 = closest piece in the NorthWest direction
      element6 = closest piece in the NorthEast direction
      element7 = closest piece in the SouthWest direction
      element8 = closest piece in the SouthEast direction


    represented as (Empty (1,1)). the list consists of the following:
      element1 = closest piece in the North direction (acc1)
      element2 = closest piece in the South direction (acc2)
      element3 = closest piece in the West direction (acc3)
      element4 = closest piece in the East direction (acc4)
      element5 = closest piece in the NorthWest direction (acc5)
      element6 = closest piece in the NorthEast direction (acc6)
      element7 = closest piece in the SouthWest direction (acc7)
      element8 = closest piece in the SouthEast direction (acc8)




-}





{-Add function specifications for monad functions???
choosePiece 
showBoard
main
play
-}




{-
rocked

  |(Piece King (8,5) Black ) && (Piece Rock (8,8) Black ) && 
    Empty (8,5) && Empty (8,6) && Empty(8,7) = True then move...... 
  |(Piece King (8,5) Black ) && (Piece Rock (8,1) Black ) && 
    Empty (8,2) && Empty (8,3) = True then move .....
  |(Piece King (1,5) White ) && (Piece Rock (1,8) White ) && 
    Empty (1,6) White && Empty (1,7) = True then move...... 
  |(Piece King (1,5) White ) && (Piece Rock (1,1) White ) && 
    Empty (1,2) && Empty (1,3) && Empty (1,4) = True then move...... 
  | otherwise .......

-}

{- showBoard board
  takes the current board and print it to the terminal
  SIDE EFFECTS: prints out the current board
-}

showBoard :: Board -> IO ()
showBoard board = do
  print(showBoardAux board 1)
  print(showBoardAux board 2)
  print(showBoardAux board 3)
  print(showBoardAux board 4)
  print(showBoardAux board 5)
  print(showBoardAux board 6)
  print(showBoardAux board 7)
  print(showBoardAux board 8)


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


{-main 
 main is only called to start a game of chess
 used to give our play function the correct starting value, the initialBoard and the color White
-}
main :: IO ()
main = play (initialBoard, White)


{-play (gameState, color1)
  play is the gameloop which will continued until the game is over
  SIDE EFFECTS: depending of how different possible events turns out different print statements will be displayed, if the game aint over the function will take two user inputs which if valid inputs then will update the gameState and change the color1 to the oppositeColor and then call itself with the new updated arguments.
-}
play :: (Board,PColor) -> IO ()
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