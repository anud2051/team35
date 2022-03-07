module Data (Board,Square(..),Position,PColor(..),PType(..),initialBoard)where


{- Board represents a full 64 square chessboard
  where each square of the board can be either empty or containing a piece
  INVARIANT: lenght of Board == 64
-}
type Board = [Square]
{- Square represents how a square looks like if it is empty or containing a piece
  a empty square represents by Empty position
  a non empty square represents of a Piece ptype pos pcolor with the type ptype, postion pos and color pcolor
-}
data Square = Piece PType Position PColor  | Empty Position deriving (Show, Eq, Read)
{-
    Position is used to determine the position on the board.
    A Position (a,b) represents a postion on the board with a representing where in the North-South direction  a piece is located and with b representing where in the West-East direction  a piece is located on the board
    INVARIANT: Both Int can only have values between 1-8
-}
type Position = (Int,Int) 
{-PColor represents the two possible colors of the pieces
  Black represents the color black and White represents the color white
-}
data PColor = Black | White deriving (Show, Eq, Read)
{-PType is all the different types of pieces in a game of chess
  Pawn represents a pawn, Knight represents a knight, Bishop represents a bishop, Rock represents a rock, Queen represents a queen and King represents a king
-}
data PType = Pawn | Knight | Bishop | Rock | Queen | King deriving (Show, Eq, Read)
{-initialBoard
variable which shows how the how the board looks like when you start a new game of chess. The initialboard contains of a list of squares where each square represents how that square looks like in an inital gamestate of chess
-}

initialBoard =  [Piece Rock (1,1) White, Piece Knight (1,2) White, Piece Bishop (1,3) White, Piece King (1,4) White,Piece Queen (1,5) White,Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,
                Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Piece Pawn (2,4) White,Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,
                Empty (3,1),Empty(3,2),Empty(3,3),Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Empty(4,4),Empty(4,5),Empty(4,6),Empty(4,7),
                Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
                Empty(5,8),Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Empty(6,6),Empty(6,7),Empty(6,8),
                Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Piece Pawn (7,8) Black,
                Piece Rock (8,1) Black, Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Piece Rock (8,8) Black]
