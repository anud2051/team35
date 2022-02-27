module Data (Board,Square(..),Position,PColor(..),PType(..),initialBoard)where

type Board = [Square]
data Square = Piece PType Position PColor  | Empty Position deriving (Show, Eq, Read)
{-
    Position is used to determine the position on the board.
    A Position (a,b) represents a postion on the board with the x-coordinate b and the y-coordinate a
    INVARIANT: Both Int can only have values between 1-8
-}
type Position = (Int,Int) 
data PColor = Black | White deriving (Show, Eq, Read)
data PType = Pawn | Knight | Bishop | Rock | Queen | King deriving (Show, Eq, Read)
{-initialBoard
varible which shows how the initialboard looks like
contains of a list of squares where each square represents how that square looks like in an inital gamestate looks like when
you start a new game of chess
-}

initialBoard =  [Piece Rock (1,1) White, Piece Knight (1,2) White, Piece Bishop (1,3) White, Piece Queen (1,4) White,Piece King (1,5) White,Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,
                Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Piece Pawn (2,4) White,Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,
                Empty (3,1),Empty(3,2),Empty(3,3),Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Empty(4,4),Empty(4,5),Empty(4,6),Empty(4,7),
                Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
                Empty(5,8),Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Empty(6,6),Empty(6,7),Empty(6,8),
                Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Piece Pawn (7,8) Black,
                Piece Rock (8,1) Black, Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Piece Rock (8,8) Black]
