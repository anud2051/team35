
module ValidMove (validMove,isNewPosInNp,validMovePawn,posToBoard,posToBoardWE,findNearestPieces,removePos) where
import MovePiece 
import PossibleMoves
import Data 
{-
 EXAMPLES: validMove initialBoard (Piece Pawn (2,1) White) (3,1) == True
-}
validMove :: Board -> Square -> Position -> Bool
validMove board piece@(Piece Pawn pos color) newPos = isNewPosInNp newPos validMoves
  where allMoves = possibleMoves piece 
        allSquares = posToBoardWE allMoves board
        validMoves = validMovePawn piece allSquares [] board

validMove board piece@(Piece ptype pos _ ) newPos = isNewPosInNp newPos validMoves
  where allMoves = possibleMoves piece 
        allSquares = posToBoard allMoves board
        nearestPieces = findNearestPieces piece allSquares
        validMoves = removePos piece nearestPieces allMoves 


isNewPosInNp :: Position -> [Position] -> Bool
isNewPosInNp _ [] = False
isNewPosInNp pos (x:xs) 
  | pos == x = True
  | otherwise = isNewPosInNp pos xs


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

posToBoardWE :: [Position] -> Board -> Board 
posToBoardWE [] board = []
posToBoardWE (x:xs) board = posToSquare x board: posToBoardWE xs board


{-posToBoard xs board
  creates a board of all pieces which has a position inside xs
  RETURNS: returns a board of pieces who has the positions insed xs
  EXAMPLES: posToBoard [(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(3,2),(4,2),(5,2),(6,2),(7,2),(8,2),(2,1),(1,2)] initialBoard ==
 [Piece Pawn (2,3) White,Piece Pawn (2,4) White,Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,Piece Pawn (7,2) Black,Piece Knight (8,2) Black,Piece Pawn (2,1) White,Piece Knight (1,2) White]
 -}
 --VARIANT: Length of xs
posToBoard :: [Position] -> Board -> Board 
posToBoard [] board =[]
posToBoard (x:xs) board 
    | posToSquare x board == Empty x = posToBoard xs board
    | otherwise = posToSquare x board: posToBoard xs board


{- findNearestPieces piece xs
  finds all the squares closest to the piece in every direction
  PRE: piece and every element in xs has to be at the format (Piece ptype position color)
  RETURNS: returns a list of 8 pieces where each piece represent one direction on the board, if there is no piece on the board at one direction this is
      represented as (Empty (1,1)). the order inside the list is as follows:
      element1 = closest piece in the North direction
      element2 = closest piece in the South direction
      element3 = closest piece in the West direction
      element4 = closest piece in the East direction
      element5 = closest piece in the NorthWest direction
      element6 = closest piece in the NorthEast direction
      element7 = closest piece in the SouthWest direction
      element8 = closest piece in the SouthEast direction
  EXAMPLES: findNearestPieces (Piece Rock (4,4) White) [(Piece Pawn (2,4) Black), (Piece Pawn (3,4) Black),(Piece Pawn (1,4) Black), (Piece Pawn (6,4) Black), (Piece Pawn (5,4) Black),(Piece Pawn (8,4) Black), (Piece Pawn (4,1) Black),(Piece Pawn (4,3) Black),(Piece Pawn (4,5) Black),(Piece Pawn (4,6) Black),(Piece Pawn (3,3) Black), 
  (Piece Pawn (2,2) Black),(Piece Pawn (5,5) Black),(Piece Pawn (7,7) Black), (Piece Pawn (8,8) Black),(Piece Pawn (6,6) Black), (Piece Pawn (2,6) Black),(Piece Pawn (3,5) Black), (Piece Pawn (1,7) Black), (Piece Pawn (6,2) Black),(Piece Pawn (5,3) Black), (Piece Pawn (7,1) Black)]  ==
  [Piece Pawn (3,4) Black,Piece Pawn (5,4) Black,Piece Pawn (4,3) Black,Piece Pawn (4,5) Black,Piece Pawn (3,3) Black,Piece Pawn (3,5) Black,Piece Pawn (5,3) Black,Piece Pawn (5,5) Black]
-}
findNearestPieces :: Square -> Board -> Board
findNearestPieces _ [] = []
findNearestPieces piece ((Piece piecetype2 (x2,y2) color2):xs) = findNearestPiecesAux piece ((Piece piecetype2 (x2,y2) color2):xs) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1))


{- findNearestPieces piece xs acc1 acc2 acc3 acc4 acc5 acc6 acc7 acc8
  finds all the squares closest to the piece in every direction
  PRE: piece and every element in xs has to be at the format (Piece ptype position color)
  RETURNS: returns a list of 8 pieces where each piece represent one direction on the board, if there is no piece on the board at one direction this is
      represented as (Empty (1,1)). the list consists of the following:
      element1 = closest piece in the North direction (acc1)
      element2 = closest piece in the South direction (acc2)
      element3 = closest piece in the West direction (acc3)
      element4 = closest piece in the East direction (acc4)
      element5 = closest piece in the NorthWest direction (acc5)
      element6 = closest piece in the NorthEast direction (acc6)
      element7 = closest piece in the SouthWest direction (acc7)
      element8 = closest piece in the SouthEast direction (acc8)
  EXAMPLES: findNearestPiecesAux (Piece Rock (4,4) White) [(Piece Pawn (2,4) Black), (Piece Pawn (3,4) Black),(Piece Pawn (1,4) Black), (Piece Pawn (6,4) Black), (Piece Pawn (5,4) Black),(Piece Pawn (8,4) Black), (Piece Pawn (4,1) Black),(Piece Pawn (4,3) Black),(Piece Pawn (4,5) Black),(Piece Pawn (4,6) Black),(Piece Pawn (3,3) Black), (Piece Pawn (2,2) Black),(Piece Pawn (5,5) Black),(Piece Pawn (7,7) Black), (Piece Pawn (8,8) Black),(Piece Pawn (6,6) Black), (Piece Pawn (2,6) Black),(Piece Pawn (3,5) Black), (Piece Pawn (1,7) Black), (Piece Pawn (6,2) Black),(Piece Pawn (5,3) Black), (Piece Pawn (7,1) Black)] (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) ==
  [Piece Pawn (3,4) Black,Piece Pawn (5,4) Black,Piece Pawn (4,3) Black,Piece Pawn (4,5) Black,Piece Pawn (3,3) Black,Piece Pawn (3,5) Black,Piece Pawn (5,3) Black,Piece Pawn (5,5) Black]
  
  findNearestPiecesAux (Piece Pawn (2,1) White) [Piece Rock (1,1) White, Piece Pawn (2,2) White] (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) (0,Empty (1,1)) ==
  [Piece Rock (1,1) White,Empty (1,1),Empty (1,1),Piece Pawn (2,2) White,Empty (1,1),Empty (1,1),Empty (1,1),Empty (1,1)]
-}
findNearestPiecesAux :: Square -> Board -> (Int, Square) -> (Int, Square) -> (Int, Square) -> (Int, Square) -> (Int, Square) -> (Int, Square) -> (Int, Square) -> (Int, Square) -> Board
findNearestPiecesAux _ [] acc1 acc2 acc3 acc4 acc5 acc6 acc7 acc8 = [(snd acc1), (snd acc2), (snd acc3), (snd acc4), (snd acc5), (snd acc6), (snd acc7), (snd acc8)]
findNearestPiecesAux (Piece piecetype1 (x1,y1) color1) (piece@(Piece piecetype2 (x2,y2) color2):xs) acc1 acc2 acc3 acc4 acc5 acc6 acc7 acc8
-- 1-North positions 
  | y1 == y2 && x1 > x2 = findNearestPiecesAux (Piece piecetype1 (x1,y1) color1) xs (updateAcc acc1 x1 x2 piece)  acc2 acc3 acc4 acc5 acc6 acc7 acc8 

-- 2 -South positions
  | y1 == y2 && x1 < x2 = findNearestPiecesAux (Piece piecetype1 (x1,y1) color1) xs acc1  (updateAcc acc2 x1 x2 piece) acc3 acc4 acc5 acc6 acc7 acc8 

--3- West positions
  | y1 > y2 && x1 == x2 = findNearestPiecesAux (Piece piecetype1 (x1,y1) color1) xs acc1 acc2 (updateAcc acc3 y1 y2 piece) acc4 acc5 acc6 acc7 acc8 

--4-East positions
  | y1 < y2 && x1 == x2 =  findNearestPiecesAux (Piece piecetype1 (x1,y1) color1) xs acc1 acc2 acc3 (updateAcc acc4 y1 y2 piece) acc5 acc6 acc7 acc8

--5- NorthWest positions
  | y1 > y2 && x1 > x2 = findNearestPiecesAux (Piece piecetype1 (x1,y1) color1) xs acc1 acc2 acc3 acc4 (updateAcc acc5 x1 x2 piece) acc6 acc7 acc8

-- 6-NorthEast positions
  | y1 < y2 && x1 > x2 = findNearestPiecesAux (Piece piecetype1 (x1,y1) color1) xs acc1 acc2 acc3 acc4 acc5 (updateAcc acc6 y1 y2 piece) acc7 acc8

-- 7-SouthWest positions
  | y1 > y2 && x1 < x2 = findNearestPiecesAux (Piece piecetype1 (x1,y1) color1) xs acc1 acc2 acc3 acc4 acc5 acc6 (updateAcc acc7 y1 y2 piece) acc8
-- 8-SouthEast positions
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

{-removePos piece xs positions


EXAMPLES: removePos (Piece Queen (4,4) White) [(Piece Pawn (2,4) Black), (Piece Pawn (4,2) Black), (Piece Pawn (7,1) White)] (possibleMoves (Piece Queen (4,4) White)) == 
[(4,5),(4,6),(4,7),(4,8),(5,5),(6,6),(7,7),(8,8),(5,4),(6,4),(7,4),(8,4),(5,3),(6,2),(4,3),(4,2),(3,3),(2,2),(1,1),(3,4),(2,4),(3,5),(2,6),(1,7)]
*Main> 
-}


removePos :: Square -> [Square] -> [Position] -> [Position]
removePos _ [] positions = positions
removePos piece ((Empty _):xs) positions = removePos piece xs positions

removePos piece (x:xs) positions = removePos piece xs (removeInvalidMoves piece x positions)


removeInvalidMoves :: Square -> Square -> [Position] -> [Position]
removeInvalidMoves _ _ [] = [] 
removeInvalidMoves piece1@(Piece ptype (x1, y1) White) piece2@(Piece _ (x2, y2) White) (pos@(x3,y3):xs)

  | y1 == y2 && x1 > x2 = if y3 == y1 && x3 <= x2 then 
  removeInvalidMoves piece1 piece2 xs
  else removeInvalidMoves piece1 piece2 xs

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

