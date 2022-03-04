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
{- move functions idé for each pice taken from https://okpanico.wordpress.com/2018/02/08/haskell-118-un-po-di-monadi-6/ -}
possibleMoves (Piece King (x,y) _ ) = filter onBoard
    
    [(x-1,y+1), -- NorthEast
    (x,y+1), -- East
    (x+1,y+1), -- SouthEast
    (x+1,y), -- South
    (x+1,y-1), -- SouthWest
    (x,y-1),-- West
    (x-1,y),-- North
    (x-1,y-1)] -- NorthWest

    where onBoard (x,y) = x `elem` [1..8] && y `elem` [1..8]


possibleMoves (Piece Queen (x,y) _ ) = filter onBoard
    
    [(x,y+1),(x,y+2),(x,y+3),(x,y+4),(x,y+5),(x,y+6),(x,y+7), -- East
    (x+1,y+1),(x+2,y+2),(x+3,y+3),(x+4,y+4),(x+5,y+5),(x+6,y+6),(x+7,y+7), -- SouthEast
    (x+1,y),(x+2,y),(x+3,y),(x+4,y),(x+5,y),(x+6,y),(x+7,y), -- South
    (x+1,y-1),(x+2,y-2),(x+3,y-3),(x+4,y-4),(x+5,y-5),(x+6,y-6),(x+7,y-7), -- SouthWest
    (x,y-1),(x,y-2),(x,y-3),(x,y-4),(x,y-5),(x,y-6),(x,y-7), -- West
    (x-1,y-1),(x-2,y-2),(x-3,y-3),(x-4,y-4),(x-5,y-5),(x-6,y-6),(x-7,y-7), -- NorthWest
    (x-1,y),(x-2,y),(x-3,y),(x-4,y),(x-5,y),(x-6,y),(x-7,y), -- North
    (x-1,y+1),(x-2,y+2),(x-3,y+3),(x-4,y+4),(x-5,y+5),(x-6,y+6),(x-7,y+7)] -- NorthEast

    where onBoard (x,y) = x `elem` [1..8] && y `elem` [1..8]


possibleMoves (Piece Bishop (x,y) _ ) = filter onBoard

    [(x+1,y+1),(x+2,y+2),(x+3,y+3),(x+4,y+4),(x+5,y+5),(x+6,y+6),(x+7,y+7), -- SouthEast
    (x+1,y-1),(x+2,y-2),(x+3,y-3),(x+4,y-4),(x+5,y-5),(x+6,y-6),(x+7,y-7), -- SouthWest
    (x-1,y-1),(x-2,y-2),(x-3,y-3),(x-4,y-4),(x-5,y-5),(x-6,y-6),(x-7,y-7), -- NorthWest
    (x-1,y+1),(x-2,y+2),(x-3,y+3),(x-4,y+4),(x-5,y+5),(x-6,y+6),(x-7,y+7)] -- NorthEast

    where onBoard (x,y) = x `elem` [1..8] && y `elem` [1..8]

--check so that the diagonal is on the board
possibleMoves pawn@(Piece Pawn (x,y) Black)
  | y == 8 = filter onBoard [(x-1,y),(x-2,y),(x-1,y-1)]
  | y == 1 = filter onBoard [(x-1,y),(x-2,y),(x-1,y+1)]
  | otherwise = filter onBoard [(x-1,y),(x-2,y),(x-1,y-1),(x-1,y+1)]
    where onBoard (x,y) = x `elem` [1..8] && y `elem` [1..8]
possibleMoves pawn@(Piece Pawn (x,y) White)
  | y == 8 = filter onBoard [(x+1,y),(x+2,y),(x+1,y-1)]
  | y == 1 = filter onBoard [(x+1,y),(x+2,y),(x+1,y+1)]
  | otherwise = filter onBoard [(x+1,y),(x+2,y),(x+1,y-1),(x+1,y+1)]
    where onBoard (x,y) = x `elem` [1..8] && y `elem` [1..8]


possibleMoves (Piece Rock (x,y) _ ) = filter onBoard

    [(x,y+1),(x,y+2),(x,y+3),(x,y+4),(x,y+5),(x,y+6),(x,y+7), -- East
    (x+1,y),(x+2,y),(x+3,y),(x+4,y),(x+5,y),(x+6,y),(x+7,y), -- South
    (x,y-1),(x,y-2),(x,y-3),(x,y-4),(x,y-5),(x,y-6),(x,y-7), -- West
    (x-1,y),(x-2,y),(x-3,y),(x-4,y),(x-5,y),(x-6,y),(x-7,y)] -- North

    where onBoard (x,y) = x `elem` [1..8] && y `elem` [1..8]

possibleMoves (Piece Knight (x,y) _ ) = filter onBoard

    [(x+1,y+2), -- up, right
    (x+2,y-1), -- right, down
    (x-1,y-2), -- down, left
    (x-2,y+1), -- left, up
    (x-1,y+2), -- upp, left
    (x+2,y+1), -- right, up
    (x+1,y-2), -- down, right
    (x-2,y-1)] -- Left, down
    
    where onBoard (x,y) = x `elem` [1..8] && y `elem` [1..8]
