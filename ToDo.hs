
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