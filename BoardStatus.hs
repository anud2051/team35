module BoardStatus (queening, choosePiece, whereIsKing, isCheck,isCheckMate) where
import ValidMove 
import PossibleMoves
import Data 
import MovePiece

queening (Piece Pawn (x1, y1) color) (x2, y2)
  | color == White && x2 == 8 = True
  | color == Black && x2 == 1 = True
  | otherwise = False

queening _ _ = False

choosePiece (move, color) = do
  newPiece <- getLine 
  if newPiece == "Bishop" ||newPiece  == "Rock" || newPiece == "Knight" || newPiece == "Queen"
  then return (Piece (read newPiece :: PType) move color)
  else choosePiece (move, color)

whereIsKing (Empty _:xs) piece@(Piece King pos2 color2) = whereIsKing xs piece
whereIsKing ((Piece King pos1 color1):xs) piece@(Piece King pos2 color2) 
  | color1 == color2 = pos1
  | otherwise = whereIsKing xs piece
whereIsKing ((Piece _ pos1 _):xs) piece@(Piece King pos2 color2) = whereIsKing xs piece

isCheckMate king@(Piece King (x1,y1) color) board = isCheckMateAux king board ((x1,y1):validMoves)
  where allMoves = possibleMoves king
        allSquares = posToBoard allMoves board
        nearestPieces = findNearestPieces king allSquares
        validMoves = removePos king nearestPieces allMoves 

isCheckMateAux _ _ [] = True
isCheckMateAux king@(Piece King pos color) board (x:xs) = isCheck (Piece King x color) testBoard && isCheckMateAux king board xs
 where testBoard = movePiece king x board




isCheck (Piece King (x1, y1) White) board = isNewPosInNp (x1,y1) (isCheckAux (onlyPieces board White) board)
isCheck (Piece King (x1, y1) Black) board = isNewPosInNp (x1,y1) (isCheckAux (onlyPieces board Black) board)

isCheckAux [] _ = []
isCheckAux (piece@(Piece Pawn pos color):xs) board = isCheckAux xs board ++ validMoves
  where allMoves = possibleMoves piece 
        allSquares = posToBoardWE allMoves board
        validMoves = validMovePawn piece allSquares [] board

isCheckAux (piece:xs) board = isCheckAux xs board ++ validMoves
  where allMoves = possibleMoves piece
        allSquares = posToBoard allMoves board
        nearestPieces = findNearestPieces piece allSquares
        validMoves = removePos piece nearestPieces allMoves 

onlyPieces [] _ = []
onlyPieces (Empty _ : xs) White = onlyPieces xs White
onlyPieces (piece@(Piece ptype pos color):xs) White 
  | color == White = onlyPieces xs White
  | otherwise = piece : onlyPieces xs White

onlyPieces (Empty _ : xs) Black = onlyPieces xs Black
onlyPieces (piece@(Piece ptype pos color):xs) Black
  | color == Black = onlyPieces xs Black
  | otherwise = piece : onlyPieces xs Black
