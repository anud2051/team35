module PossibleMoves (possibleMoves) where
import Data 


{-possibleMoves piece
  finds all possible moves for a given piece
  PRE: piece == (Piece ptype pos color)
  RETURNS: a list of positions where the given piece could possibly move.
  EXAMPLES: possibleMoves (Piece King (4,4) White) == [(3,5),(4,5),(5,5),(5,4),(5,3),(4,3),(3,4),(3,3)]
             possibleMoves (Piece Pawn (3,2) White) == [(4,2),(5,2),(4,1),(4,3)] 
            possibleMoves (Piece Knight (8,2) Black) == [(6,3),(7,4),(6,1)]
-}
{- move functions idé for each pice inspired/taken from https://okpanico.wordpress.com/2018/02/08/haskell-118-un-po-di-monadi-6/ -}
possibleMoves (Piece King (x,y) _ ) = filter onBoard
    
    [(x-1,y+1), 
    (x,y+1), 
    (x+1,y+1),
    (x+1,y),
    (x+1,y-1),
    (x,y-1),
    (x-1,y),
    (x-1,y-1)] 

    where onBoard (x,y) = x `elem` [1..8] && y `elem` [1..8]


possibleMoves (Piece Queen (x,y) _ ) = filter onBoard
    
    [(x,y+1),(x,y+2),(x,y+3),(x,y+4),(x,y+5),(x,y+6),(x,y+7), 
    (x+1,y+1),(x+2,y+2),(x+3,y+3),(x+4,y+4),(x+5,y+5),(x+6,y+6),(x+7,y+7), 
    (x+1,y),(x+2,y),(x+3,y),(x+4,y),(x+5,y),(x+6,y),(x+7,y),
    (x+1,y-1),(x+2,y-2),(x+3,y-3),(x+4,y-4),(x+5,y-5),(x+6,y-6),(x+7,y-7),
    (x,y-1),(x,y-2),(x,y-3),(x,y-4),(x,y-5),(x,y-6),(x,y-7),
    (x-1,y-1),(x-2,y-2),(x-3,y-3),(x-4,y-4),(x-5,y-5),(x-6,y-6),(x-7,y-7), 
    (x-1,y),(x-2,y),(x-3,y),(x-4,y),(x-5,y),(x-6,y),(x-7,y),
    (x-1,y+1),(x-2,y+2),(x-3,y+3),(x-4,y+4),(x-5,y+5),(x-6,y+6),(x-7,y+7)]

    where onBoard (x,y) = x `elem` [1..8] && y `elem` [1..8]


possibleMoves (Piece Bishop (x,y) _ ) = filter onBoard

    [(x+1,y+1),(x+2,y+2),(x+3,y+3),(x+4,y+4),(x+5,y+5),(x+6,y+6),(x+7,y+7), 
    (x+1,y-1),(x+2,y-2),(x+3,y-3),(x+4,y-4),(x+5,y-5),(x+6,y-6),(x+7,y-7), 
    (x-1,y-1),(x-2,y-2),(x-3,y-3),(x-4,y-4),(x-5,y-5),(x-6,y-6),(x-7,y-7), 
    (x-1,y+1),(x-2,y+2),(x-3,y+3),(x-4,y+4),(x-5,y+5),(x-6,y+6),(x-7,y+7)] 

    where onBoard (x,y) = x `elem` [1..8] && y `elem` [1..8]


possibleMoves (Piece Pawn (x,y) color) 
  |color == Black = filter onBoard [(x-1,y),(x-2,y),(x-1,y-1),(x-1,y+1)]
  |otherwise = filter onBoard [(x+1,y),(x+2,y),(x+1,y-1),(x+1,y+1)]
    where onBoard (x,y) = x `elem` [1..8] && y `elem` [1..8]



possibleMoves (Piece Rock (x,y) _ ) = filter onBoard

    [(x,y+1),(x,y+2),(x,y+3),(x,y+4),(x,y+5),(x,y+6),(x,y+7), 
    (x+1,y),(x+2,y),(x+3,y),(x+4,y),(x+5,y),(x+6,y),(x+7,y), 
    (x,y-1),(x,y-2),(x,y-3),(x,y-4),(x,y-5),(x,y-6),(x,y-7), 
    (x-1,y),(x-2,y),(x-3,y),(x-4,y),(x-5,y),(x-6,y),(x-7,y)] 

    where onBoard (x,y) = x `elem` [1..8] && y `elem` [1..8]

possibleMoves (Piece Knight (x,y) _ ) = filter onBoard

    [(x+1,y+2), 
    (x+2,y-1), 
    (x-1,y-2), 
    (x-2,y+1), 
    (x-1,y+2), 
    (x+2,y+1), 
    (x+1,y-2), 
    (x-2,y-1)] 
    
    where onBoard (x,y) = x `elem` [1..8] && y `elem` [1..8]
