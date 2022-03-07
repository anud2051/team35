module ValidMove (isValidMove,isPosInPositions,validMovePawn,posToBoard,posToBoardWE,findNearestPieces,findValidMoves,allValidMoves) where
import MovePiece 
import PossibleMoves
import Data 


{- allValidMoves square board
  finds all valid moves for a square on the current board
  RETURNS: a list of valid positions for square on board
  EXAMPLES: allValidMoves (Piece Pawn (2,1) White) initialBoard == [(4,1),(3,1)]
            allValidMoves (Piece Rock (6,8) Black) [Piece Rock (1,1) White, Piece Knight (1,2) White, Empty (1,3), Piece King (1,4) White,Piece Queen (1,5) White,Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White, Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Empty (2,4),Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White, Empty (3,1),Empty(3,2),Empty(3,3),Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8), Empty(4,1),Empty(4,2),Empty(4,3),Piece Pawn (4,4) White,Empty(4,5),Piece Bishop (4,6) White,Empty(4,7), Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7), Piece Pawn (5,8) Black,Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Empty(6,6),Empty(6,7),Piece Rock (6,8) Black,Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Empty(7,8),Piece Rock (8,1) Black, Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Empty(8,8)] == [(7,8),(8,8),(6,7),(6,6),(6,5),(6,4),(6,3),(6,2),(6,1)]
-}
allValidMoves :: Square -> Board -> [(Int, Int)]
allValidMoves piece@(Piece Pawn pos color) board = validMoves
    where allMoves = possibleMoves piece 
          allSquares = posToBoardWE allMoves board
          validMoves = validMovePawn piece allSquares [] board
allValidMoves piece board = validMoves
    where allMoves = possibleMoves piece 
          allSquares = posToBoard allMoves board
          nearestPieces = findNearestPieces piece allSquares
          validMoves = findValidMoves piece nearestPieces allMoves 

{-isValidMove board piece newPos
  checks if a move is valid or not
  RETURNS: True if it is valid to move a piece to a newPos at the given board, else False
  EXAMPLES: isValidMove initialBoard (Piece Pawn (2,1) White) (3,1) == True
          isValidMove initialBoard (Piece Rock (8,8) Black) (6,8) == False
-}

isValidMove :: Board -> Square -> Position -> Bool
isValidMove board piece@(Piece ptype pos color ) newPos =
  isPosInPositions newPos validMoves 
  where 
    validMoves = allValidMoves piece board


{-isPosInPositions pos xs
  checks if pos occurs inside xs
  RETURNS: True if pos occurs inside xs, else False
  EXAMPLES: isPosInPositions (4,4) [(2,2),(4,4),(2,6)] == True
            isPosInPositions (4,3) [(2,2),(4,4),(2,6)] == False
-}
--VARIANT: length of xs
isPosInPositions :: Position -> [Position] -> Bool
isPosInPositions _ [] = False
isPosInPositions pos (x:xs) 
  | pos == x = True
  | otherwise = isPosInPositions pos xs



{-validMovePawn piece xs newPositions board
  finds out all valid moves a pawn can make
  PRE: piece == (Piece Pawn pos color)
RETURNS: returns newPositions where each position inside newPositions is a valid move for the piece at the board, the valid moves depends of the elements inside xs
EXAMPLES: validMovePawn (Piece Pawn (2,1) White) [Empty (3,1), Empty (4,1), Empty (2,2)] [] initialBoard == [(4,1),(3,1)]
          validMovePawn (Piece Pawn (6,3) White) [Piece Pawn (7,3) Black, Piece Bishop (8,3) Black, Piece Pawn (7,2) Black,Piece Pawn (7,4) Black] [] initialBoard == [(7,4),(7,2)]
-}
--VARIANT: Length of xs
validMovePawn _ [] newPositions _ = newPositions
validMovePawn piece@(Piece Pawn (x1,y1) White) ((Empty (x2, y2)):xs) newPositions board             
  | (posToSquare (x1+1,y1) board) == Empty (x1+1,y1) && y1 == y2 = validMovePawn piece xs ((x2,y2):newPositions) board
  | otherwise = validMovePawn piece xs newPositions board
validMovePawn piece@(Piece Pawn (x1,y1) White) ((Piece ptype (x2, y2) color):xs) newPositions board 
  | x1 + 1 == x2 && y1 == y2 = validMovePawn piece xs newPositions board
  | x1 + 2 == x2 && y1 == y2 = validMovePawn piece xs newPositions board 
  | x1 + 1 == x2 && y1 + 1 == y2 && color == White = validMovePawn piece xs newPositions board
  | x1 + 1 == x2 && y1 - 1 == y2 && color == White = validMovePawn piece xs newPositions board
  | otherwise = validMovePawn piece xs ((x2,y2):newPositions) board
validMovePawn piece@(Piece Pawn (x1,y1) Black) ((Empty (x2, y2)):xs) newPositions board             
  | (posToSquare (x1-1,y1) board) == Empty (x1-1,y1) && y1 == y2 = validMovePawn piece xs ((x2,y2):newPositions) board
  | otherwise = validMovePawn piece xs newPositions board
validMovePawn piece@(Piece Pawn (x1,y1) Black) ((Piece ptype (x2, y2) color):xs) newPositions board 
  | x1 - 1 == x2 && y1 == y2 = validMovePawn piece xs newPositions board
  | x1 - 2 == x2 && y1 == y2 = validMovePawn piece xs newPositions board 
  | x1 - 1 == x2 && y1 + 1 == y2 && color == Black = validMovePawn piece xs newPositions board
  | x1 -  1 == x2 && y1 - 1 == y2 && color == Black = validMovePawn piece xs newPositions board
  | otherwise = validMovePawn piece xs ((x2,y2):newPositions) board


{-posToBoardWE xs board
  converts all positions inside xs to squares
  RETURNS: a list containing all positions inside xs converted into the matching square at the given board
EXAMPLES: posToBoardWE [(1,1),(4,4), (8,8)] initialBoard == [Piece Rock (1,1) White,Empty (4,4),Piece Rock (8,8) Black]
          posToBoardWE [(1,3),(3,4), (7,8),(6,8)] initialBoard ==[Piece Bishop (1,3) White,Empty (3,4),Piece Pawn (7,8) Black,Empty (6,8)]
-}
--VARIANT Lenght of xs
posToBoardWE :: [Position] -> Board -> [Square]
posToBoardWE [] board = []
posToBoardWE (x:xs) board = posToSquare x board: posToBoardWE xs board


{-posToBoard xs board
  converts all positions inside xs to squares
  RETURNS: returns a list of pieces who has the positions inside xs at the given board
  EXAMPLES: posToBoard [(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(3,2),(4,2),(5,2),(6,2),(7,2),(8,2),(2,1),(1,2)] initialBoard == [Piece Pawn (2,3) White,Piece Pawn (2,4) White,Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,Piece Pawn (7,2) Black,Piece Knight (8,2) Black,Piece Pawn (2,1) White,Piece Knight (1,2) White]
 -}
 --VARIANT: Length of xs
posToBoard :: [Position] -> Board -> [Square]
posToBoard [] board =[]
posToBoard (x:xs) board 
    | posToSquare x board == Empty x = posToBoard xs board
    | otherwise = posToSquare x board: posToBoard xs board


{- findNearestPieces piece xs
  finds all the squares closest to the piece in every direction
  PRE: piece and every element in xs has to be at the format (Piece ptype position color)
  RETURNS: returns a list of 8 pieces closest to the given piece at the given board where each piece represent one direction on the board, if there is no piece on the board at one direction this is
      represented as (Empty (1,1)). 
  EXAMPLES: findNearestPieces (Piece Rock (4,4) White) [(Piece Pawn (2,4) Black), (Piece Pawn (3,4) Black),(Piece Pawn (1,4) Black), (Piece Pawn (6,4) Black), (Piece Pawn (5,4) Black),(Piece Pawn (8,4) Black), (Piece Pawn (4,1) Black),(Piece Pawn (4,3) Black),(Piece Pawn (4,5) Black),(Piece Pawn (4,6) Black),(Piece Pawn (3,3) Black),(Piece Pawn (2,2) Black),(Piece Pawn (5,5) Black),(Piece Pawn (7,7) Black), (Piece Pawn (8,8) Black),(Piece Pawn (6,6) Black), (Piece Pawn (2,6) Black),(Piece Pawn (3,5) Black), (Piece Pawn (1,7) Black), (Piece Pawn (6,2) Black),(Piece Pawn (5,3) Black), (Piece Pawn (7,1) Black)]  ==[Piece Pawn (3,4) Black,Piece Pawn (5,4) Black,Piece Pawn (4,3) Black,Piece Pawn (4,5) Black,Piece Pawn (3,3) Black,Piece Pawn (3,5) Black,Piece Pawn (5,3) Black,Piece Pawn (5,5) Black]
-}
--VARIANT Length of xs
findNearestPieces :: Square -> Board -> [Square]
findNearestPieces _ [] = []
findNearestPieces piece ((Piece piecetype2 (x2,y2) color2):xs) = findNearestPiecesAux piece ((Piece piecetype2 (x2,y2) color2):xs) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1))


{- findNearestPieces piece xs acc1 acc2 acc3 acc4 acc5 acc6 acc7 acc8
  finds all the squares closest to the piece in every direction
  PRE: piece and every element in xs has to be at the format (Piece ptype position color)
  RETURNS: returns a list of the 8 closest squares to the given piece where each square represent one direction on the board, if there is no piece on the board at one direction this is
      represented as (Empty (1,1)). the list consists of the following:
      element0 = acc1
      element1 = acc2
      element2 = acc3
      element3 = acc4
      element4 = acc5
      element5 = acc6
      element6 = acc7
      element7 = acc8
  EXAMPLES: findNearestPiecesAux (Piece Rock (4,4) White) [(Piece Pawn (2,4) Black), (Piece Pawn (3,4) Black),(Piece Pawn (1,4) Black), (Piece Pawn (6,4) Black), (Piece Pawn (5,4) Black),(Piece Pawn (8,4) Black), (Piece Pawn (4,1) Black),(Piece Pawn (4,3) Black),(Piece Pawn (4,5) Black),(Piece Pawn (4,6) Black),(Piece Pawn (3,3) Black), (Piece Pawn (2,2) Black),(Piece Pawn (5,5) Black),(Piece Pawn (7,7) Black), (Piece Pawn (8,8) Black),(Piece Pawn (6,6) Black), (Piece Pawn (2,6) Black),(Piece Pawn (3,5) Black), (Piece Pawn (1,7) Black), (Piece Pawn (6,2) Black),(Piece Pawn (5,3) Black), (Piece Pawn (7,1) Black)] (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) ==[Piece Pawn (3,4) Black,Piece Pawn (5,4) Black,Piece Pawn (4,3) Black,Piece Pawn (4,5) Black,Piece Pawn (3,3) Black,Piece Pawn (3,5) Black,Piece Pawn (5,3) Black,Piece Pawn (5,5) Black]
  
  findNearestPiecesAux (Piece Pawn (2,1) White) [Piece Rock (1,1) White, Piece Pawn (2,2) White] (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) == [Piece Rock (1,1) White,Empty (1,1),Empty (1,1),Piece Pawn (2,2) White,Empty (1,1),Empty (1,1),Empty (1,1),Empty (1,1)]
-}
--VARIANT Length of xs
findNearestPiecesAux :: Square -> Board -> (Int, Square) -> (Int, Square) -> (Int, Square) -> (Int, Square) -> (Int, Square) -> (Int, Square) -> (Int, Square) -> (Int, Square) -> [Square]
findNearestPiecesAux _ [] acc1 acc2 acc3 acc4 acc5 acc6 acc7 acc8 = [(snd acc1), (snd acc2), (snd acc3), (snd acc4), (snd acc5), (snd acc6), (snd acc7), (snd acc8)]
findNearestPiecesAux (Piece piecetype1 (x1,y1) color1) (piece@(Piece piecetype2 (x2,y2) color2):xs) acc1 acc2 acc3 acc4 acc5 acc6 acc7 acc8

  | y1 == y2 && x1 > x2 = findNearestPiecesAux (Piece piecetype1 (x1,y1) color1) xs (updateAcc acc1 x1 x2 piece)  acc2 acc3 acc4 acc5 acc6 acc7 acc8 

  | y1 == y2 && x1 < x2 = findNearestPiecesAux (Piece piecetype1 (x1,y1) color1) xs acc1  (updateAcc acc2 x1 x2 piece) acc3 acc4 acc5 acc6 acc7 acc8 

  | y1 > y2 && x1 == x2 = findNearestPiecesAux (Piece piecetype1 (x1,y1) color1) xs acc1 acc2 (updateAcc acc3 y1 y2 piece) acc4 acc5 acc6 acc7 acc8 

  | y1 < y2 && x1 == x2 =  findNearestPiecesAux (Piece piecetype1 (x1,y1) color1) xs acc1 acc2 acc3 (updateAcc acc4 y1 y2 piece) acc5 acc6 acc7 acc8

  | y1 > y2 && x1 > x2 = findNearestPiecesAux (Piece piecetype1 (x1,y1) color1) xs acc1 acc2 acc3 acc4 (updateAcc acc5 x1 x2 piece) acc6 acc7 acc8

  | y1 < y2 && x1 > x2 = findNearestPiecesAux (Piece piecetype1 (x1,y1) color1) xs acc1 acc2 acc3 acc4 acc5 (updateAcc acc6 y1 y2 piece) acc7 acc8

  | y1 > y2 && x1 < x2 = findNearestPiecesAux (Piece piecetype1 (x1,y1) color1) xs acc1 acc2 acc3 acc4 acc5 acc6 (updateAcc acc7 y1 y2 piece) acc8

  | y1 < y2 && x1 < x2 =  findNearestPiecesAux (Piece piecetype1 (x1,y1) color1) xs acc1 acc2 acc3 acc4 acc5 acc6 acc7 (updateAcc acc8 y1 y2 piece)


{-UpdateAcc acc x1 x2 piece
    updates the acc to the correct value
    RETURNS: a tuple containing the value of either acc or (x1-x2) along with piece
    EXAMPLES: updateAcc (0,Empty (1,1))  4 3 (Piece King (3,2) Black) == (1,Piece King (3,2) Black)
              updateAcc (1,(Piece Pawn (3,2) Black))  4 1 (Piece King (1,2) Black) == (1,Piece Pawn (3,2) Black)
              updateAcc (0,Empty (1,1)) 5 8 (Piece King (6,8) Black) == ((-3),Piece King (6,8) Black)
              updateAcc ((-3),Piece King (6,8) Black) 5 6 (Piece King (6,6) Black) == (-1,Piece King (6,6) Black)
-}
updateAcc ::  (Int, Square) -> Int -> Int -> Square -> (Int,Square)
updateAcc acc x1 x2 piece 
  | fst acc == 0 = (x1 - x2, piece)
  | compare x1 x2 == 0
  = if fst acc < (x1 - x2) then acc else (x1 - x2, piece)
  | fst acc < (x1 - x2) = (x1 - x2, piece)
  | otherwise = acc
  where compare x1 x2 =if x1>x2 then 0 else 1

{-findValidMoves piece xs positions
  finds all valid moves for a given piece
  PRE: piece /= (Piece Pawn pos color) && piece/= Empty pos
  RETURNS: a list of all valid moves a given piece can move to, removes all invalid moves from postions by using xs
EXAMPLES: findValidMoves (Piece Queen (4,4) White) [(Piece Pawn (2,4) Black), (Piece Pawn (4,2) Black), (Piece Pawn (7,1) White)] (possibleMoves (Piece Queen (4,4) White)) == [(4,5),(4,6),(4,7),(4,8),(5,5),(6,6),(7,7),(8,8),(5,4),(6,4),(7,4),(8,4),(5,3),(6,2),(4,3),(4,2),(3,3),(2,2),(1,1),(3,4),(2,4),(3,5),(2,6),(1,7)]
        findValidMoves (Piece Rock (2,4) White) [(Piece Pawn (3,4) Black), (Piece Pawn (2,2) Black), (Piece Pawn (2,6) White)] (possibleMoves (Piece Rock (2,4) White)) == [(2,5),(3,4),(2,3),(2,2),(1,4)]

-}
--VARIANT Length of xs

findValidMoves :: Square -> [Square] -> [Position] -> [Position]
findValidMoves _ [] positions = positions
findValidMoves piece ((Empty _):xs) positions = findValidMoves piece xs positions
findValidMoves piece (x:xs) positions = findValidMoves piece xs (removeInvalidMoves piece x positions)


{-removeInvalidMoves piece1 piece2 xs
  removes invalid moves from xs
  PRE: piece1 /= Empty pos, piece1 /= (Piece Pawn ptype pos color), piece2 /= Empty pos
  RETURNS: returns an updated list of positions, depending how piece1 and piece2 correlates to each other some values from xs will be removed from the returned list of positions
  EXAMPLES: removeInvalidMoves (Piece Queen (4,4) White) (Piece Rock (5,4) White) (possibleMoves (Piece Queen (4,4) White)) == [(4,5),(4,6),(4,7),(4,8),(5,5),(6,6),(7,7),(8,8),(5,3),(6,2),(7,1),(4,3),(4,2),(4,1),(3,3),(2,2),(1,1),(3,4),(2,4),(1,4),(3,5),(2,6),(1,7)]
          removeInvalidMoves (Piece Queen (4,4) White) (Piece Rock (4,5) White) ([(4,5),(4,6),(4,7),(4,8),(5,5),(6,6),(7,7),(8,8),(5,3),(6,2),(7,1),(4,3),(4,2),(4,1),(3,3),(2,2),(1,1),(3,4),(2,4),(1,4),(3,5),(2,6),(1,7)]) == [(5,5),(6,6),(7,7),(8,8),(5,3),(6,2),(7,1),(4,3),(4,2),(4,1),(3,3),(2,2),(1,1),(3,4),(2,4),(1,4),(3,5),(2,6),(1,7)]
          removeInvalidMoves (Piece Queen (4,4) White) (Piece Rock (5,5) White) [(5,5),(6,6),(7,7),(8,8),(5,3),(6,2),(7,1),(4,3),(4,2),(4,1),(3,3),(2,2),(1,1),(3,4),(2,4),(1,4),(3,5),(2,6),(1,7)] == [(5,3),(6,2),(7,1),(4,3),(4,2),(4,1),(3,3),(2,2),(1,1),(3,4),(2,4),(1,4),(3,5),(2,6),(1,7)]

      
-}
removeInvalidMoves :: Square -> Square -> [Position] -> [Position]
removeInvalidMoves _ _ [] = [] 
removeInvalidMoves piece1@(Piece ptype (x1, y1) White) piece2@(Piece _ (x2, y2) White) (pos@(x3,y3):xs)

  | y1 == y2 && x1 > x2 = if y3 == y1 && x3 <= x2 then 
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 == y2 && x1 < x2 = if y3 == y1 && x3 >= x2 then       
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 > y2 && x1 == x2 = if x3 == x1 && y3 <= y2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 < y2 && x1 == x2 = if x3 == x1 && y3 >= y2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 < y2 && x1 < x2 = if y3 >= y2 && x3 >= x2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 < y2 && x1 > x2 = if y3 >= y2 && x3 <= x2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 > y2 && x1 < x2 = if y3 <= y2 && x3 >= x2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 > y2 && x1 > x2 = if y3 <= y2 && x3 <= x2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

removeInvalidMoves piece1@(Piece _ (x1, y1) White) piece2@(Piece _ (x2, y2) Black) (pos@(x3,y3):xs)
  | y1 == y2 && x1 > x2 = if y3 == y1 && x3 < x2 then 
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 == y2 && x1 < x2 = if y3 == y1 && x3 > x2 then 
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 > y2 && x1 == x2 = if x3 == x1 && y3 < y2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 < y2 && x1 == x2 = if x3 == x1 && y3 > y2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 < y2 && x1 < x2 = if y3 > y2 && x3 > x2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 < y2 && x1 > x2 = if y3 > y2 && x3 < x2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 > y2 && x1 < x2 = if y3 < y2 && x3 > x2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 > y2 && x1 > x2 = if y3 < y2 && x3 < x2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

removeInvalidMoves piece1@(Piece _ (x1, y1) Black) piece2@(Piece _ (x2, y2) White) (pos@(x3,y3):xs)
  | y1 == y2 && x1 > x2 = if y3 == y1 && x3 < x2 then 
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 == y2 && x1 < x2 = if y3 == y1 && x3 > x2 then 
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 > y2 && x1 == x2 = if x3 == x1 && y3 < y2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 < y2 && x1 == x2 = if x3 == x1 && y3 > y2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 < y2 && x1 < x2 = if y3 > y2 && x3 > x2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 < y2 && x1 > x2 = if y3 > y2 && x3 < x2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 > y2 && x1 < x2 = if y3 < y2 && x3 > x2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 > y2 && x1 > x2 = if y3 < y2 && x3 < x2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

removeInvalidMoves piece1@(Piece _ (x1, y1) Black) piece2@(Piece _ (x2, y2) Black) (pos@(x3,y3):xs)
  | y1 == y2 && x1 > x2 = if y3 == y1 && x3 <= x2 then 
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 == y2 && x1 < x2 = if y3 == y1 && x3 >= x2 then 
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 > y2 && x1 == x2 = if x3 == x1 && y3 <= y2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 < y2 && x1 == x2 = if x3 == x1 && y3 >= y2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 < y2 && x1 < x2 = if y3 >= y2 && x3 >= x2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 < y2 && x1 > x2 = if y3 >= y2 && x3 <= x2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 > y2 && x1 < x2 = if y3 <= y2 && x3 >= x2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

  | y1 > y2 && x1 > x2 = if y3 <= y2 && x3 <= x2 then
  removeInvalidMoves piece1 piece2 xs
  else pos : removeInvalidMoves piece1 piece2 xs

