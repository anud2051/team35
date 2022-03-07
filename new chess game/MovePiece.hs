module MovePiece (movePiece,posToSquare,replaceWithEmpty) where
import Data 

{-movePiece square newPos board 
  moves a piece from one square to another square in the board
  PRE: square == (Piece ptype position color)
  RETURNS: an updated board where the position of the given square has been changed to empty and the square at the position of the given newPos has been updated containing the piece of given square
  EXAMPLES: movePiece (Piece Rock (1,1) White) (1,2) [Piece Rock (1,1) White, Piece Knight (1,2) White, Piece Bishop (1,3) White, Piece Queen (1,5) White] == [Empty (1,1),Piece Rock (1,2) White,Piece Bishop (1,3) White,Piece Queen (1,5) White]      
            movePiece (Piece Pawn (7,2) Black) (6,6) [Empty(6,6),Empty(6,7),Empty(6,8), Piece Pawn (7,1) Black,Piece Pawn (7,2) Black] == [Piece Pawn (6,6) Black,Empty (6,7),Empty (6,8),Piece Pawn (7,1) Black,Empty (7,2)]
-}
movePiece :: Square -> Position -> Board -> Board
movePiece square newPos board = replace square posSquare replaceEmpty
 where 
  posSquare = posToSquare newPos board 
  replaceEmpty = replaceWithEmpty square board


{- replaceWithEmpty square xs
  replaces the square a piece has left with an empty square 
  RETURNS: a Board where the position of square in the board xs is replaced with Empty
  EXAMPLES: replaceWithEmpty (Piece Rock (1,1) White) [Piece Rock (1,1) White,Piece Knight (1,2) White,Piece Bishop (1,3) White] == [Empty (1,1),Piece Knight (1,2) White,Piece Bishop (1,3) White]
-}
--VARIANT: length of xs
replaceWithEmpty :: Square -> Board -> Board
replaceWithEmpty _ [] = []
replaceWithEmpty piece@(Piece piecetype (x1,y1) color) (x:xs)
  | x == piece = Empty (x1,y1) : (replaceWithEmpty piece xs)
  | otherwise = x:(replaceWithEmpty piece xs) 

{- posToSquare pos1 xs
  finds the piece on a specific position
  RETURNS: the Square of the position pos1 in the board xs
  EXAMPLES: posToSquare (1,1) [Piece Rock (1,1) White,Piece Knight (1,2) White,Piece Bishop (1,3) White] == Piece Rock (1,1) White
-}
--VARIANT: lenght of xs

posToSquare :: Position -> Board -> Square
posToSquare pos1 ((Empty pos2):xs) 
  | pos1 == pos2 = Empty pos2
  | otherwise = posToSquare pos1 xs
posToSquare pos1 ((Piece piecetype pos2 color):xs) 
  | pos1 == pos2 = Piece piecetype pos2 color
  | otherwise = posToSquare pos1 xs 


{-replace piece1 piece2 xs
  replaces a square (piece2) with another square (piece1)
  RETURNS: returns a board where piece1 has replaced piece2 in the given board xs 
  EXAMPLES: replace (Piece Queen (8,5) Black) (Piece Rock (1,1) White) [Piece Rock (1,1) White,Piece Knight (1,2) White,Piece Bishop (1,3) White] == [Piece Queen (1,1) Black,Piece Knight (1,2) White,Piece Bishop (1,3) White]
-}
--VARIANT: lenght of xs
replace::  Square -> Square -> Board -> Board
replace _ _ [] = []
replace piece1@(Piece piecetype1 (x1,y1) color1) piece2@(Piece piecetype2 (x2,y2) color2) (x:xs)
  | x == piece2 = (Piece piecetype1 (x2,y2) color1) : replace piece1 piece2 xs 
  | otherwise = x: replace piece1 piece2 xs 
replace square1@(Piece piecetype1 (x1,y1) color1) square2@(Empty (x2,y2) ) (x:xs)
  | x == square2 = (Piece piecetype1 (x2,y2) color1) : replace square1 square2 xs 
  | otherwise = x: replace square1 square2 xs 