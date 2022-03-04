module BoardStatus (isQueening, choosePiece,isCheck,isCheckMate,isStaleMate) where
import ValidMove 
import PossibleMoves
import Data 
import MovePiece

-- Rad 53, 163

{-isStaleMate board king
  checks if it is stalemate
  PRE: king == (Piece King pos color)
  RETURNS: True if it is stalemate, else False
  EXAMPLES: isStaleMate initialBoard (Piece King (1,4) White) == False
            isStaleMate initialBoard (Piece King (4,4) Black) == False
-}
isStaleMate:: Board-> Square-> Bool 
isStaleMate board king@(Piece King pos color) = isCheckMateAux king board validMoves && isStaleMateAux board king
   where 
         validMoves = allValidMoves king board
         remainingPieces = onlyPieces board (oppositeColor color)

{-isStaleMateAux board king
  checks whether the king is the only piece remaining or if the remaining pieces cant move
  PRE: king == (Piece King pos color)
  RETURNS: True if the king is the only piece remaining or the remaining pieces cant move, else False
  EXAMPLES: isStaleMateAux initialBoard (Piece King (4,4) Black) == False
            isStaleMateAux initialBoard (Piece King (1,4) Black) == False
-}
isStaleMateAux :: Board -> Square -> Bool
isStaleMateAux board king@(Piece King pos color) 
  | x == king && null xs = True
  | remainingMoves (x:xs) board == [] = True 
  | otherwise = False
    where 
      (x:xs) = onlyPieces board (oppositeColor color)

{-remainingMoves xs board
  finds all valid moves for all remaining pieces xs on the board
  PRE: xs cant contain an empty square (Empty pos)
  RETURNS: a list of positions of all valid moves for all of the pieces inside xs
  EXAMPLES: remainingMoves [Piece Pawn (2,4) White, Piece Pawn (7,6) Black, Piece King (1,4) White] initialBoard == [(4,4),(3,4),(5,6),(6,6)]
            remainingMoves [Piece King (1,4) White, Piece Rock (8,8) Black, Piece Bishop (1,3) White] initialBoard == []

-}
remainingMoves :: [Square] -> Board -> [Position]
remainingMoves [] _ = []
remainingMoves (pawn@(Piece Pawn pos color):xs) board = validMoves ++ remainingMoves xs board
  where 
      validMoves = allValidMoves pawn board
remainingMoves (x:xs) board = validMoves ++ remainingMoves xs board
  where 
        validMoves = allValidMoves x board

{- isQueening piece (x2, y2)
  checks if a pawn reaches the opposite side of the board
  RETURNS: True if piece == (Piece Pawn pos color) and (color == White and x2 == 8) or (color == Black and x2 == 1), else False
  EXAMPLES: isQueening (Piece Pawn (7, 7) White) (8, 7) == True
            isQueening (Piece Pawn (2, 7) Black) (1, 7) == True
            isQueening (Piece Pawn (4, 5) Black) (3, 5) == False
-}
isQueening :: Square -> Position -> Bool
isQueening (Piece Pawn (x1, y1) color) (x2, y2)
  | color == White && x2 == 8 = True
  | color == Black && x2 == 1 = True
  | otherwise = False
isQueening _ _ = False


choosePiece (move, color) = do
  print "E.g Rock"
  newPiece <- getLine 
  if newPiece == "Bishop" || newPiece  == "Rock" || newPiece == "Knight" || newPiece == "Queen"
  then return (Piece (read newPiece :: PType) move color)
  else choosePiece (move, color)





{- isCheckMate king@(Piece King (x1,y1) color) board
  Checks if a checkmate has occured
  PRE: king = (Piece King (x1,y1) color)
  RETURNS: True if it is checkmate, else False
  EXAMPLES: isCheckMate (Piece King (1,4) White) initialBoard == False
            isCheckMate (Piece King (8,4) Black) schoolBoard7 == True
-}
isCheckMate :: Square -> Board -> Bool
isCheckMate king@(Piece King (x1,y1) color) board = isCheckMateAux king board ((x1,y1):validMoves) && noPossibleMove king (onlyPieces board (oppositeColor color)) board
  where allMoves = possibleMoves king
        allSquares = posToBoard allMoves board
        nearestPieces = findNearestPieces king allSquares
        validMoves = findValidMoves king nearestPieces allMoves 

{-noPossibleMove king xs board
  checks if there is any move that could be done to avoid checkmate
  PRE: all square inside xs has to have the same color and cant be empty, king == Piece King pos color
  RETURNS: True if there is no possible move to avoid checkmate, else False
  EXAMPLES: noPossibleMove (Piece King (3,4) Black) [Piece Pawn (7,2) Black, Piece Rock (8,8) Black] initialBoard == True
            noPossibleMove (Piece King (1,4) Black) [Piece Pawn (7,2) Black, Piece Rock (8,8) Black] initialBoard == False
-}
--VARIANT: Length of xs
noPossibleMove :: Square -> [Square] -> Board -> Bool
noPossibleMove _ [] _ = True 
noPossibleMove king (pawn@(Piece Pawn pos color):xs) board = noPossibleMoveAux king pawn validMoves board && noPossibleMove king xs board 
  where allMoves = possibleMoves pawn
        allSquares = posToBoardWE allMoves board
        validMoves = validMovePawn pawn allSquares [] board

noPossibleMove king (x:xs) board = noPossibleMoveAux king x validMoves board && noPossibleMove king xs board 
  where allMoves = possibleMoves x
        allSquares = posToBoard allMoves board
        nearestPieces = findNearestPieces x allSquares
        validMoves = findValidMoves x nearestPieces allMoves

{-noPossibleMoveAux king piece xs board
  checks if no of a piece validmoves xs can save the king from being in check
  PRE: xs can only contain validmoves for a piece, piece /= Empty pos, king == Piece King pos color
  RETURNS: True if none of the piece moves inside xs can make so that king no longer is in check, else False
  EXAMPLES: noPossibleMoveAux (Piece King (1,4) Black) (Piece Rock (8,8) Black) [] initialBoard == True
            noPossibleMoveAux (Piece King (1,4) Black) (Piece Queen (2,4) Black) [(2,3),(2,5),(3,4),(4,4),(5,4),(6,4),(1,5),(1,3)] initialBoard == False
-}
--VARIANT: length of xs
noPossibleMoveAux :: Square -> Square ->[Position]-> Board -> Bool
noPossibleMoveAux _ _ [] _ = True
noPossibleMoveAux king piece (x:xs) board 
  | king == piece = True
  | otherwise = isCheck king testBoard && noPossibleMoveAux king piece xs board 
    where testBoard = movePiece piece x board


{- isCheckMateAux king board xs
  checks if the king is in check at his current position and all of his validmoves
  PRE: king == (Piece King pos color)
  RETURNS: True if the king is in check and all of his validmoves is in check, else False
  EXAMPLES: isCheckMateAux (Piece King (1,4) White) initialBoard [(1,4)] == False
            isCheckMateAux (Piece King (8,4) White) initialBoard [(8,4)] == True
-}
--VARIANT: length of xs
isCheckMateAux :: Square -> Board -> [Position] -> Bool
isCheckMateAux _ _ [] = True
isCheckMateAux king@(Piece King pos color) board (x:xs) 
  | pos == x = isCheck king testBoard && isCheckMateAux (Piece King x color) board xs
  | otherwise =  isCheck (Piece King x color) testBoard && isCheckMateAux (Piece King x color) board xs
    where testBoard = movePiece king x board 

