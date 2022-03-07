module BoardStatus (isQueening, choosePiece,isCheck,isCheckMate,isStaleMate,whereIsKing,oppositePieces,oppositeColor) where
import ValidMove 
import PossibleMoves
import Data 
import MovePiece


{-isStaleMate board king
  checks if it is stalemate on the current board
  PRE: king == (Piece King pos color)
  RETURNS: True if it is stalemate on the current board meaning you cant move your king or any other pieces, else False
  EXAMPLES: isStaleMate initialBoard (Piece King (1,4) White) == False
            isStaleMate staleMateTestBoard1 (Piece King (1,1) White) == True
-}

isStaleMate:: Board-> Square-> Bool 
isStaleMate board king@(Piece King pos color) = isCheckMateAux king board validMoves && isStaleMateAux board king
   where 
         validMoves = allValidMoves king board

{-isStaleMateAux board king
  checks whether the king is the only piece remaining or if the remaining pieces cant move
  PRE: king == (Piece King pos color)
  RETURNS: True if the king is the only piece remaining on the board or the remaining pieces on the board cant move, else False
  EXAMPLES: isStaleMateAux initialBoard (Piece King (4,4) Black) == False
            isStaleMateAux initialBoard (Piece King (1,4) Black) == False
-}

isStaleMateAux :: Board -> Square -> Bool
isStaleMateAux board king@(Piece King pos color) 
  | x == king && null xs = True
  | remainingMoves (x:xs) board == [] = True 
  | otherwise = False
    where 
      (x:xs) = oppositePieces board (oppositeColor color)

{-remainingMoves xs board
  finds all valid moves for all remaining pieces xs on the board
  PRE: xs cant contain an empty square (Empty pos)
  RETURNS: a list of positions of all valid moves for all of the pieces inside xs at the board
  EXAMPLES: remainingMoves [Piece Pawn (2,4) White, Piece Pawn (7,6) Black, Piece King (1,4) White] initialBoard == [(4,4),(3,4),(5,6),(6,6)]
            remainingMoves [Piece King (1,4) White, Piece Rock (8,8) Black, Piece Bishop (1,3) White] initialBoard == []

-}
remainingMoves :: [Square] -> Board -> [Position]
remainingMoves [] _ = []
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

{- whereIsKing xs color2
  Checks where the King is located at the current board
  PRE: requires a King to be located at the current board
  RETURNS: the position of the king with the color color2 at the board xs
  EXAMPLES: whereIsKing initialBoard White == (1,4)
            whereIsKing initialBoard Black == (8,4)
-}
--VARIANT: lenght of xs
whereIsKing :: Board -> PColor -> Position
whereIsKing (Empty _:xs) color2 = whereIsKing xs color2
whereIsKing ((Piece King pos1 color1):xs) color2
  | color1 == color2 = pos1
  | otherwise = whereIsKing xs color2
whereIsKing ((Piece _ pos1 _):xs) color2  = whereIsKing xs color2

{-choosePiece (move,color)
  lets the player choose which piece the player wants to turn the pawn into.
  PRE: move :: Position, color :: PType
  RETURNS: returns an IO Piece where the Piece is the newPiece the player writes along with move and color
  SIDE EFFECTS: Prints "E.g Rock,Bishop,Knight,Queen", takes an input from the user which is supposed to be the name of the wanted piece.
-}

choosePiece :: (Position, PColor) -> IO Square
choosePiece (move, color) = do
  print "E.g Rock,Bishop,Knight,Queen"
  newPiece <- getLine 
  if newPiece == "Bishop" || newPiece  == "Rock" || newPiece == "Knight" || newPiece == "Queen"
  then return (Piece (read newPiece :: PType) move color)
  else choosePiece (move, color)



{- isCheckMate king@(Piece King (x1,y1) color) board
  Checks if a checkmate has occured
  PRE: king = (Piece King (x1,y1) color)
  RETURNS: True if it is checkmate meaning the king at the board is checkmated, else False
  EXAMPLES: isCheckMate (Piece King (1,4) White) initialBoard == False
            isCheckMate (Piece King (8,4) Black) schoolBoard7 == True
-}

isCheckMate :: Square -> Board -> Bool
isCheckMate king@(Piece King pos color) board = isCheckMateAux king board (pos:validMoves) && noPossibleMove king (oppositePieces board (oppositeColor color)) board
  where 
        validMoves = allValidMoves king board 

{-noPossibleMove king xs board
  checks if there is any move that could be done to avoid checkmate
  PRE: all square inside xs has to have the same color as the king and cant be empty, king == Piece King pos color
  RETURNS: True if there is no valid moves for any of the pieces inside xs to avoid the king from being checkmate at the given board, else False
  EXAMPLES: noPossibleMove (Piece King (3,4) Black) [Piece Pawn (7,2) Black, Piece Rock (8,8) Black] initialBoard == True
            noPossibleMove (Piece King (1,4) Black) [Piece Pawn (7,2) Black, Piece Rock (8,8) Black] initialBoard == False
-}
--VARIANT: Length of xs
noPossibleMove :: Square -> [Square] -> Board -> Bool
noPossibleMove _ [] _ = True 
noPossibleMove king (piece:xs) board = noPossibleMoveAux king piece validMoves board && noPossibleMove king xs board 
  where 
        validMoves = allValidMoves piece board

{-noPossibleMoveAux king piece xs board
  checks if none of a piece validmoves xs can save the king from being in check
  PRE: xs can only contain validmoves for the given piece, piece /= Empty pos, king == Piece King pos color
  RETURNS: True if none of the piece moves inside xs can make so that king no longer is in check at the given board, else False
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
  checks if the king is in check at his current position and at all of his valid moves
  PRE: king == (Piece King pos color)
  RETURNS: True if the king is in check and all of his validmoves xs is in check at the given board, else False
  EXAMPLES: isCheckMateAux (Piece King (1,4) White) initialBoard [(1,4)] == False
            isCheckMateAux (Piece King (8,4) White) initialBoard [(8,4)] == True
-}
--VARIANT: length of xs
isCheckMateAux :: Square -> Board -> [Position] -> Bool
isCheckMateAux _ _ [] = True
isCheckMateAux king@(Piece King pos color) board (x:xs) = isCheck (Piece King x color) testBoard && isCheckMateAux (Piece King pos color) board xs
    where testBoard = movePiece king x board


{-oppositeColor color
  gives the opposite color
  RETURNS: if color == White then Black else White
  EXAMPLES: oppositeColor White == Black
            oppositeColor Black == White
-}
oppositeColor:: PColor -> PColor
oppositeColor color
  | color == White = Black
  | otherwise = White


{- isCheck king@(Piece King pos color) board
  checks if the given King is in check
  PRE: king == (Piece King pos color)
  RETURNS: True if the king is in check at the board, else False
  EXAMPLES: isCheck (Piece King (1,4) White) initialBoard == False
            isCheck (Piece King (6,6) White) initialBoard == True
-}

isCheck :: Square -> Board -> Bool
isCheck (Piece King pos White) board = isPosInPositions pos (isCheckAux (oppositePieces board White) board)
isCheck (Piece King pos Black) board = isPosInPositions pos (isCheckAux (oppositePieces board Black) board)


{- isCheckAux xs board
  concatenates all validMoves for each piece inside xs
  RETURNS: a concatenated list of all valid moves for each piece inside xs at the board
  EXAMPLES: isCheckAux [Piece Pawn (2,1) White, Piece Pawn (2,4) White] initialBoard == [(4,4),(3,4),(4,1),(3,1)]
            isCheckAux [Piece Pawn (2,2) White, Piece Knight (1,2) White] initialBoard == [(3,1),(3,3),(4,2),(3,2)]
-}
--VARIANT: lenght of xs

isCheckAux :: [Square] -> Board -> [Position]
isCheckAux [] _ = []
isCheckAux (piece:xs) board =isCheckAux xs board ++ validMoves
  where 
        validMoves = allValidMoves piece board 

{- oppositePieces xs color
  extracts all pieces with the opposite color of the given color from the board xs
  RETURNS: a list of pieces where each piece inside the list is a piece with the opposite color to the given color at the given board
  EXAMPLES: oppositePieces initialBoard White == [Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Piece Pawn (7,8) Black,Piece Rock (8,1) Black,Piece Knight (8,2) Black,Piece Bishop (8,3) Black,Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Piece Rock (8,8) Black]
            oppositePieces initialBoard Black == [Piece Rock (1,1) White,Piece Knight (1,2) White,Piece Bishop (1,3) White,Piece King (1,4) White,Piece Queen (1,5) White,Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Piece Pawn (2,4) White,Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White]
-}
--VARIANT: lenght of xs

oppositePieces :: Board -> PColor -> [Square]
oppositePieces [] _ = []
oppositePieces (Empty _ : xs) White = oppositePieces xs White
oppositePieces (piece@(Piece ptype pos color):xs) White 
  | color == White = oppositePieces xs White
  | otherwise = piece : oppositePieces xs White

oppositePieces (Empty _ : xs) Black = oppositePieces xs Black
oppositePieces (piece@(Piece ptype pos color):xs) Black
  | color == Black = oppositePieces xs Black
  | otherwise = piece : oppositePieces xs Black