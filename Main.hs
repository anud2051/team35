
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




{-movePiece square move board 
  moves a piece from one square to another square in the board
  PRE: square == (Piece ptype position color)
  RETURNS: an updated board where the position of the given square has been changed to empty and the square at the position of the given move has been updated containing the piece of given square
  EXAMPLES: movePiece (Piece Rock (1,1) White) (1,2) [Piece Rock (1,1) White, Piece Knight (1,2) White, Piece Bishop (1,3) White, Piece Queen (1,5) White] == [Empty (1,1),Piece Rock (1,2) White,Piece Bishop (1,3) White,Piece Queen (1,5) White]      
            movePiece (Piece Pawn (7,2) Black) (6,6) [Empty(6,6),Empty(6,7),Empty(6,8), Piece Pawn (7,1) Black,Piece Pawn (7,2) Black] == [Piece Pawn (6,6) Black,Empty (6,7),Empty (6,8),Piece Pawn (7,1) Black,Empty (7,2)]
-}
movePiece :: Square -> Position -> Board -> Board
movePiece square move board = replace square posSquare replaceEmpty
 where 
  posSquare = posToSquare move board 
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

{- posToSquare pos xs
  finds the piece on a specific position
  PRE: pos1 has to exist in the board xs
  RETURNS: the Square of the position pos in the board xs
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


{- whereIsKing xs color2
  Checks where the King is located on the current board
  PRE: requires a King to be located at the current board
  RETURNS: the position of the king with the color color2
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


{- isCheckMate king@(Piece King (x1,y1) color) board
  Checks if a checkmate has occured
  PRE: king = (Piece King (x1,y1) color)
  RETURNS: True if it is checkmate, else False
  EXAMPLES: isCheckMate (Piece King (1,4) White) initialBoard == False
            isCheckMate (Piece King (8,4) White) initialBoard == True
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



{- isCheck king@(Piece King pos color) board
  checks if the given King is in check
  PRE: king == (Piece King pos color)
  RETURNS: True if the king is in check, else False
  EXAMPLES: isCheck (Piece King (1,4) White) initialBoard == False
            isCheck (Piece King (6,6) White) initialBoard == True
-}
isCheck :: Square -> Board -> Bool
isCheck (Piece King pos White) board = isPosInPositions pos (isCheckAux (onlyPieces board White) board)
isCheck (Piece King pos Black) board = isPosInPositions pos (isCheckAux (onlyPieces board Black) board)


{- isCheckAux xs board
  concatenates all validMoves for each piece inside xs
  RETURNS: a concatenated list of all validMoves for each piece inside xs
  EXAMPLES: isCheckAux [Piece Pawn (2,1) White, Piece Pawn (2,4) White] initialBoard == [(4,4),(3,4),(4,1),(3,1)]
            isCheckAux [Piece Pawn (2,2) White, Piece Knight (1,2) White] initialBoard == [(3,1),(3,3),(4,2),(3,2)]
-}
--VARIANT: lenght of xs
isCheckAux :: [Square] -> Board -> [Position]
isCheckAux [] _ = []
isCheckAux (piece@(Piece Pawn pos color):xs) board =isCheckAux xs board ++ validMoves
  where allMoves = possibleMoves piece 
        allSquares = posToBoardWE allMoves board
        validMoves = validMovePawn piece allSquares [] board
isCheckAux (piece:xs) board =isCheckAux xs board ++ validMoves
  where allMoves = possibleMoves piece
        allSquares = posToBoard allMoves board
        nearestPieces = findNearestPieces piece allSquares
        validMoves = findValidMoves piece nearestPieces allMoves 

{- onlyPieces xs color
  extracts all pieces with the opposite color of the given color from the board xs
  RETURNS: a list of Squares where each square inside the list is a piece with the opposite color to the given color
  EXAMPLES: onlyPieces initialBoard White == [Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Piece Pawn (7,8) Black,Piece Rock (8,1) Black,Piece Knight (8,2) Black,Piece Bishop (8,3) Black,Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Piece Rock (8,8) Black]
            onlyPieces initialBoard Black == [Piece Rock (1,1) White,Piece Knight (1,2) White,Piece Bishop (1,3) White,Piece Queen (1,4) White,Piece King (1,5) White,Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Piece Pawn (2,4) White,Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White]
-}
--VARIANT: lenght of xs
onlyPieces :: Board -> PColor -> [Square]
onlyPieces [] _ = []
onlyPieces (Empty _ : xs) White = onlyPieces xs White
onlyPieces (piece@(Piece ptype pos color):xs) White 
  | color == White = onlyPieces xs White
  | otherwise = piece : onlyPieces xs White

onlyPieces (Empty _ : xs) Black = onlyPieces xs Black
onlyPieces (piece@(Piece ptype pos color):xs) Black
  | color == Black = onlyPieces xs Black
  | otherwise = piece : onlyPieces xs Black












{-isValidMove board piece newPos
  checks if a move is valid or not
  RETURNS: True if the move is valid, else False
 EXAMPLES: isValidMove initialBoard (Piece Pawn (2,1) White) (3,1) == True
          isValidMove initialBoard (Piece Rock (8,8) Black) (6,8) == False
-}
isValidMove :: Board -> Square -> Position -> Bool
isValidMove board piece@(Piece Pawn pos color) newPos = isPosInPositions newPos validMoves
  where allMoves = possibleMoves piece 
        allSquares = posToBoardWE allMoves board
        validMoves = validMovePawn piece allSquares [] board

isValidMove board piece@(Piece ptype pos _ ) newPos 
  |ptype == King = isPosInPositions newPos validMoves  && isKingNotNextToKing board piece newPos  && not (isCheck (Piece King newPos color) board)
  |otherwise = isPosInPositions newPos validMoves
  where allMoves = possibleMoves piece 
        allSquares = posToBoard allMoves board
        nearestPieces = findNearestPieces piece allSquares
        validMoves = findValidMoves piece nearestPieces allMoves 


{-isKingNotNextToKing board king newPos
  checks if the king will be next to the enemy king at the new position newPos or not
  PRE: king == (Piece King pos color)
  RETURNS: True if the king is not next to the enemy king at the new position newPos, else False
  EXAMPLES: isKingNextToKing initialBoard (Piece King (6,4) White) (7,4) == True
            isKingNextToKing initialBoard (Piece King (6,4) White) (5,4) == False
            isKingNextToKing initialBoard (Piece King (3,3) Black) (2,3) == True
-}
isKingNotNextToKing :: Board -> Square -> Position  -> Bool
isKingNotNextToKing board king@(Piece King pos color) newPos  = not( isPosInPositions enemyKingPos validMoves )
  where
    testBoard = movePiece king newPos board
    enemyKingPos = whereIsKing testBoard (oppositeColor color)
    validMoves = allValidMoves (Piece King newPos color) testBoard
{-isPosInPositions pos xs
  checks if pos occurs inside xs
  RETURNS: True if pos occurs inside xs, else False
  EXAMPLES: isPosInPositions (4,4) [(2,2),(4,4),(2,6)] == True
            isPosInPositions (4,3) [(2,2),(4,4),(2,6)] == False
-}
--VARIANT: length of xs
isPosInPositions :: Position -> [Position] -> Bool
isPosInPositions _ [] = False
isPosInPositions pos (x:xs) 
  | pos == x = True
  | otherwise = isPosInPositions pos xs

{-validMovePawn piece xs newPositions board
  finds out all valid moves a pawn can make
  PRE: piece == (Piece Pawn pos color)
RETURNS: a list of positions where each position is a position the Pawn are allowed to move to
EXAMPLES: validMovePawn (Piece Pawn (2,1) White) [Empty (3,1), Empty (4,1), Empty (2,2)] [] initialBoard == [(4,1),(3,1)]
          validMovePawn (Piece Pawn (6,3) White) [Piece Pawn (7,3) Black, Piece Bishop (8,3) Black, Piece Pawn (7,2) Black,Piece Pawn (7,4) Black] [] initialBoard == [(7,4),(7,2)]
-}
--VARIANT: Length of xs
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


{-posToBoardWE xs board
  converts all positions inside xs to squares
  RETURNS: a list containing all positions inside xs converted into the matching square
EXAMPLES: posToBoardWE [(1,1),(4,4), (8,8)] initialBoard == [Piece Rock (1,1) White,Empty (4,4),Piece Rock (8,8) Black]
          posToBoardWE [(1,3),(3,4), (7,8),(6,8)] initialBoard ==[Piece Bishop (1,3) White,Empty (3,4),Piece Pawn (7,8) Black,Empty (6,8)]
-}
--VARIANT Lenght of xs
posToBoardWE :: [Position] -> Board -> [Square]
posToBoardWE [] board = []
posToBoardWE (x:xs) board = posToSquare x board: posToBoardWE xs board


{-posToBoard xs board
  converts all positions inside xs to squares
  RETURNS: returns a list of pieces who has the positions inside xs
  EXAMPLES: posToBoard [(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(3,2),(4,2),(5,2),(6,2),(7,2),(8,2),(2,1),(1,2)] initialBoard ==
 [Piece Pawn (2,3) White,Piece Pawn (2,4) White,Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,Piece Pawn (7,2) Black,Piece Knight (8,2) Black,Piece Pawn (2,1) White,Piece Knight (1,2) White]
 -}
 --VARIANT: Length of xs
posToBoard :: [Position] -> Board -> [Square]
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
--VARIANT Length of xs
findNearestPieces :: Square -> Board -> [Square]
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
--VARIANT Length of xs
findNearestPiecesAux :: Square -> Board -> (Int, Square) -> (Int, Square) -> (Int, Square) -> (Int, Square) -> (Int, Square) -> (Int, Square) -> (Int, Square) -> (Int, Square) -> [Square]
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

{-findValidMoves piece xs positions
  finds all valid moves for a given piece
  PRE: piece /= (Piece Pawn pos color) && piece/= Empty pos
  RETURNS: a list of all valid moves a given piece can move to, removes all invalid moves from postions by using xs
EXAMPLES: findValidMoves (Piece Queen (4,4) White) [(Piece Pawn (2,4) Black), (Piece Pawn (4,2) Black), (Piece Pawn (7,1) White)] (possibleMoves (Piece Queen (4,4) White)) == 
[(4,5),(4,6),(4,7),(4,8),(5,5),(6,6),(7,7),(8,8),(5,4),(6,4),(7,4),(8,4),(5,3),(6,2),(4,3),(4,2),(3,3),(2,2),(1,1),(3,4),(2,4),(3,5),(2,6),(1,7)]
        findValidMoves (Piece Rock (2,4) White) [(Piece Pawn (3,4) Black), (Piece Pawn (2,2) Black), (Piece Pawn (2,6) White)] (possibleMoves (Piece Rock (2,4) White)) == [(2,5),(3,4),(2,3),(2,2),(1,4)]

-}
--VARIANT Length of xs

findValidMoves :: Square -> [Square] -> [Position] -> [Position]
findValidMoves _ [] positions = positions
findValidMoves piece ((Empty _):xs) positions = findValidMoves piece xs positions

findValidMoves piece (x:xs) positions = findValidMoves piece xs (removeInvalidMoves piece x positions)

{-removeInvalidMoves piece1 piece2 xs
  removes invalid moves from xs
  PRE: piece1 /= Empty pos, piece1 /= (Piece Pawn ptype pos color), piece2 /= Empty pos
  RETURNS: returns an updated list of positions, depending how piece1 and piece2 correlates to each other
  EXAMPLES: removeInvalidMoves (Piece Queen (4,4) White) (Piece Rock (5,4) White) (possibleMoves (Piece Queen (4,4) White)) == [(4,5),(4,6),(4,7),(4,8),(5,5),(6,6),(7,7),(8,8),(5,3),(6,2),(7,1),(4,3),(4,2),(4,1),(3,3),(2,2),(1,1),(3,4),(2,4),(1,4),(3,5),(2,6),(1,7)]
          removeInvalidMoves (Piece Queen (4,4) White) (Piece Rock (4,5) White) ([(4,5),(4,6),(4,7),(4,8),(5,5),(6,6),(7,7),(8,8),(5,3),(6,2),(7,1),(4,3),(4,2),(4,1),(3,3),(2,2),(1,1),(3,4),(2,4),(1,4),(3,5),(2,6),(1,7)]) == [(5,5),(6,6),(7,7),(8,8),(5,3),(6,2),(7,1),(4,3),(4,2),(4,1),(3,3),(2,2),(1,1),(3,4),(2,4),(1,4),(3,5),(2,6),(1,7)]
          removeInvalidMoves (Piece Queen (4,4) White) (Piece Rock (5,5) White) [(5,5),(6,6),(7,7),(8,8),(5,3),(6,2),(7,1),(4,3),(4,2),(4,1),(3,3),(2,2),(1,1),(3,4),(2,4),(1,4),(3,5),(2,6),(1,7)] == [(5,3),(6,2),(7,1),(4,3),(4,2),(4,1),(3,3),(2,2),(1,1),(3,4),(2,4),(1,4),(3,5),(2,6),(1,7)]

      
-}
removeInvalidMoves :: Square -> Square -> [Position] -> [Position]
removeInvalidMoves _ _ [] = [] 
removeInvalidMoves piece1@(Piece ptype (x1, y1) White) piece2@(Piece _ (x2, y2) White) (pos@(x3,y3):xs)

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






--showBoard creates a visualization of the given board
showBoard board = do
  print(showBoardAux board 1)
  print(showBoardAux board 2)
  print(showBoardAux board 3)
  print(showBoardAux board 4)
  print(showBoardAux board 5)
  print(showBoardAux board 6)
  print(showBoardAux board 7)
  print(showBoardAux board 8)

{-pieceToChar square
  convert all different types of squares to a more readible form
  RETURNS: A tuple containing a char which represent the piecetype along with the position of the given square. '_' represents a Empty square, low case characters represent White pieces and upper-cased characters represent Black pieces.
  EXAMPLES: pieceToChar (Piece Rock (2, 1) White) == ('R',(2,1))
            pieceToChar (Empty (4, 4)) == (' ',(4,4))
            pieceToChar (Piece King (8, 4) Black) == ('k',(8,4))
-}
pieceToChar (Empty (x, y)) = (' ', (x, y))
pieceToChar (Piece Rock (x, y) White) = ('R',(x, y))
pieceToChar (Piece Knight (x, y) White) = ('N',(x, y))
pieceToChar (Piece Bishop (x, y) White) = ('B',(x, y))
pieceToChar (Piece King (x, y) White) = ('K',(x, y))
pieceToChar (Piece Queen (x, y) White) = ('Q',(x, y))
pieceToChar (Piece Pawn (x, y) White) = ('P',(x, y))
pieceToChar (Piece Rock (x, y) Black) = ('r',(x, y))
pieceToChar (Piece Knight (x, y) Black) = ('n',(x, y))
pieceToChar (Piece Bishop (x, y) Black) = ('b',(x, y))
pieceToChar (Piece King (x, y) Black) = ('k',(x, y))
pieceToChar (Piece Queen (x, y) Black) = ('q',(x, y))
pieceToChar (Piece Pawn (x, y) Black) = ('p',(x, y))

{-showBoardAux xs row
  returns a list of all squares at the given row inside the board xs
  PRE: 0 < row < 9 
  RETURNS: a list of squares which occurs at the given row in the   given board xs
  EXAMPLES:  showBoardAux initialBoard 2 == [('P',(2,1)),('P',(2,2)),('P',(2,3)),('P',(2,4)),('P',(2,5)),('P',(2,6)),('P',(2,7)),('P',(2,8))]
            showBoardAux initialBoard 3 == [(' ',(3,1)),(' ',(3,2)),(' ',(3,3)),(' ',(3,4)),(' ',(3,5)),(' ',(3,6)),(' ',(3,7)),(' ',(3,8))]

-}
--VARIANT: length of xs
showBoardAux :: Board -> Int -> [(Char,(Int,Int))]
showBoardAux [] _ = []
showBoardAux (piece@(Piece piecetype (x, y) color):xs) row 
  | x == row = pieceToChar piece: showBoardAux xs row
  | otherwise = showBoardAux xs row
showBoardAux (square@(Empty (x, y)):xs) int 
  | x == int = pieceToChar square: showBoardAux xs int
  | otherwise = showBoardAux xs int



-- Our main function which will be called to start a game of chess
main = play (initialBoard, White)
--play is the gameloop which will continued until the game is over
play (gameState, color1) = do
    showBoard gameState
    if isCheckMate (Piece King (whereIsKing gameState color1) color1) gameState then 
      putStrLn $ "Player " ++ show (oppositeColor color1) ++ " has won the game"
    --quit
    else if isStaleMate gameState (Piece King (whereIsKing gameState color1) color1) then
      putStrLn $ "Stalemate! It's a draw!"
      else do
        putStrLn $ "Which " ++ show color1 ++ " piece do you want to move?"
        piece <- getLine 
        print "Where do you want to move it?"
        move <- getLine
        if isInputCorrect piece && isInputCorrect move then 
          if isSquareEmpty (readInput piece) gameState then
            let piece1 = readInput piece
                move1 = readInput move
                pieceColor = posToColor (posToSquare piece1 gameState)
                validMove = isValidMove gameState (posToSquare piece1 gameState) move1
                whereisKing = whereIsKing (movePiece (posToSquare piece1 gameState) move1 gameState) color1
                notCheck = not (isCheck (Piece King whereisKing color1)((movePiece (posToSquare piece1 gameState) move1 gameState))) in
            if color1 == pieceColor && validMove && notCheck
            then 
              if isQueening (posToSquare piece1 gameState) move1
              then do
                newPiece <- choosePiece (move1, color1)
                play $ (movePiece newPiece move1 gameState, oppositeColor color1) 
              else play $ (movePiece (posToSquare piece1 gameState) move1 gameState, oppositeColor color1)
            else do
              print "Incorrect move" 
              play (gameState, color1)
          else do
            putStrLn "You selected an empty square"
            putStrLn "Please choose a piece!"
            play (gameState,color1)
        else do
            putStrLn "Ivalid format!"
            putStrLn "correct format: (x,y) where x,y has to be values between 1-8"
            play (gameState,color1)
      
{-isSquareNotEmpty pos board 
  checks if a sqaure a position pos at the board is empty
  RETURNS: True if the square with position pos is empty, else False
  EXAMPLES: isSquareNotEmpty (4,4) initialBoard == True
            isSquareNotEmpty (1,5) initialBoard 
-}
isSquareNotEmpty pos board 
  | posToSquare pos board == Empty pos = False 
  | otherwise = True 


{-isInputAPosition s
  checks if the given input s is in the format of a position
  RETURNS: True if s is at the format of a Position, else False
  EXAMPLES: isInputAPosition "(1,1)" == True
            isInputAPosition "(4,2)" == True
            isInputAPosition "(9,9)" == False
            isInputAPosition "abc" == False
-}
isInputAPosition :: String -> Bool
isInputAPosition s 
  |s == "(1,1)" = True
  |s == "(1,2)" = True
  |s == "(1,3)" = True
  |s == "(1,4)" = True
  |s == "(1,5)" = True
  |s == "(1,6)" = True
  |s == "(1,7)" = True
  |s == "(1,8)" = True
  |s == "(2,1)" = True
  |s == "(2,2)" = True
  |s == "(2,3)" = True
  |s == "(2,4)" = True
  |s == "(2,5)" = True
  |s == "(2,6)" = True
  |s == "(2,7)" = True
  |s == "(2,8)" = True
  |s == "(3,1)" = True  
  |s == "(3,2)" = True
  |s == "(3,3)" = True
  |s == "(3,4)" = True
  |s == "(3,5)" = True
  |s == "(3,6)" = True
  |s == "(3,7)" = True
  |s == "(3,8)" = True
  |s == "(4,1)" = True
  |s == "(4,2)" = True
  |s == "(4,3)" = True
  |s == "(4,4)" = True
  |s == "(4,5)" = True
  |s == "(4,6)" = True
  |s == "(4,7)" = True
  |s == "(4,8)" = True
  |s == "(5,1)" = True
  |s == "(5,2)" = True
  |s == "(5,3)" = True
  |s == "(5,4)" = True
  |s == "(5,5)" = True
  |s == "(5,6)" = True
  |s == "(5,7)" = True
  |s == "(5,8)" = True
  |s == "(6,1)" = True
  |s == "(6,2)" = True
  |s == "(6,3)" = True
  |s == "(6,4)" = True
  |s == "(6,5)" = True
  |s == "(6,6)" = True
  |s == "(6,7)" = True
  |s == "(6,8)" = True
  |s == "(7,1)" = True
  |s == "(7,2)" = True
  |s == "(7,3)" = True
  |s == "(7,4)" = True
  |s == "(7,5)" = True
  |s == "(7,6)" = True
  |s == "(7,7)" = True
  |s == "(7,8)" = True
  |s == "(8,1)" = True
  |s == "(8,2)" = True
  |s == "(8,3)" = True
  |s == "(8,4)" = True
  |s == "(8,5)" = True
  |s == "(8,6)" = True
  |s == "(8,7)" = True
  |s == "(8,8)" = True
  |otherwise = False
{-readInput s
  converts a string s to a Position
  RETURNS: a the string but read as a postion
  EXAMPLES: readInput "(3,3)" == (3,3)
            readInput "(2,5)" == (2,5)
-}
readInput :: String -> Position
readInput s = read s :: Position
{-posToColor piece
  extracts the color of a given piece
  PRE: piece == (Piece ptype pos color)
  RETURNS: the color (Black or White) of the given piece
  EXAMPLES: posToColor (Piece King (8,4) Black) == Black
            posToColor (Piece Pawn (2,2) White) == White
-}
posToColor (Piece ptype pos color) = color

{-oppositeColor color
  gives the opposite color
  RETURNS: if color == White then Black else White
  EXAMPLES: oppositeColor White == Black
            oppositeColor Black == White
-}
oppositeColor color
  | color == White = Black
  | otherwise = White




























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
