
import Test.HUnit
import BoardStatus
import Play
import MovePiece
import PossibleMoves
import ValidMove 
import Data
    ( PType(Bishop, Pawn, Rock, Knight, King, Queen),
      PColor(Black, White),
      Square(Empty, Piece),
      initialBoard ) 

test1 = TestCase $ assertEqual "nearestPieces"
            [Piece Pawn (3,4) Black,Piece Pawn (5,4) Black,Piece Pawn (4,3) Black,Piece Pawn (4,5) Black,Piece Pawn (3,3) Black,Piece Pawn (3,5) Black,Piece Pawn (5,3) Black,Piece Pawn (5,5) Black]
 (findNearestPieces (Piece Rock (4,4) White) [(Piece Pawn (2,4) Black), (Piece Pawn (3,4) Black),(Piece Pawn (1,4) Black), (Piece Pawn (6,4) Black), (Piece Pawn (5,4) Black),(Piece Pawn (8,4) Black), (Piece Pawn (4,1) Black),(Piece Pawn (4,3) Black),(Piece Pawn (4,5) Black),(Piece Pawn (4,6) Black),(Piece Pawn (3,3) Black), (Piece Pawn (2,2) Black),(Piece Pawn (5,5) Black),(Piece Pawn (7,7) Black), (Piece Pawn (8,8) Black),(Piece Pawn (6,6) Black), (Piece Pawn (2,6) Black),(Piece Pawn (3,5) Black), (Piece Pawn (1,7) Black), (Piece Pawn (6,2) Black),(Piece Pawn (5,3) Black), (Piece Pawn (7,1) Black)] )

test2 = let allSquares = posToBoard (possibleMoves (Piece Rock (4,4) Black)) initialBoard in  TestCase $ assertEqual "nearestPieces combained With posToBoard and possibleMoves" [Piece Pawn (2,4) White,Piece Pawn (7,4) Black,Empty (1,1),Empty (1,1),Empty (1,1),Empty (1,1),Empty (1,1),Empty (1,1)] (findNearestPieces (Piece Rock (4,4) Black) allSquares )
                                                                                                                                              
test3= TestCase $ assertEqual  "MovePiece" [Piece Rock (1,1) White,Piece Knight (1,2) White,Piece Bishop (1,3) White,Piece King (1,4) White,Piece Queen (1,5) White,Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Piece Pawn (2,4) White,Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,Empty (3,1),Empty (3,2),Empty (3,3),Empty (3,4),Empty (3,5),Empty (3,6),Empty (3,7),Empty (3,8),Empty (4,1),Empty (4,2),Empty (4,3),Empty (4,4),Empty (4,5),Empty (4,6),Empty (4,7),Piece Rock (4,8) Black,Empty (5,1),Empty (5,2),Empty (5,3),Empty (5,4),Empty (5,5),Empty (5,6),Empty (5,7),Empty (5,8),Empty (6,1),Empty (6,2),Empty (6,3),Empty (6,4),Empty (6,5),Empty (6,6),Empty (6,7),Empty (6,8),Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Piece Pawn (7,8) Black,Piece Rock (8,1) Black,Piece Knight (8,2) Black,Piece Bishop (8,3) Black,Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Empty (8,8)] (movePiece (Piece Rock (8,8) Black) (4,8) initialBoard)

test4= TestCase $ assertEqual "isValidMove " True (isValidMove  initialBoard (Piece Pawn (2,1) White) (3,1))
test5 = TestCase $ assertEqual "PawnMoveStraight" False (isValidMove  testBoard2 (Piece Pawn (7,1) Black) (6,1))

testSchool1 = TestCase $ assertEqual "isValidMove  White -Pawn (2,4) to (4,4)" True (isValidMove  initialBoard (Piece Pawn (2,4) White) (4,4))
testSchool2 = TestCase $ assertEqual "isValidMove  Black- Pawn (7,8) to (5,8)" True (isValidMove   schoolBoard1 (Piece Pawn (7,8) Black) (5,8))

testSchool3 = TestCase $ assertEqual "isValidMove  White - Bishop (1,3) to (4,6)" True (isValidMove  schoolBoard2 (Piece Bishop (1,3) White) (4,6))

testSchool4 = TestCase $ assertEqual "isValidMove  Black- Rock (8,8) to (7,8)" True (isValidMove  schoolBoard3 (Piece Rock (8,8) Black) (7,8))
testSchool5 = TestCase $ assertEqual "isValidMove  White- Queen (1,5) to (3,3)" True (isValidMove  schoolBoard4 (Piece Queen (1,5) White) (3,3))
testSchool6 = TestCase $ assertEqual "isValidMove  Black- Knight (8,7) to (6,6)" True (isValidMove  schoolBoard5 (Piece Knight (8,7) Black) (6,6))

testSchool7 = TestCase $ assertEqual "isValidMove  White- Queen (3,3) to (7,3)" True (isValidMove  schoolBoard6 (Piece Queen (3,3) White) (7,3))
testSchool8 = TestCase $ assertEqual "isCheckMate at school mate board" True (isCheckMate (Piece King (8,4) Black) schoolBoard7)
testPossibleMovesQueen = TestCase $ assertEqual "all possible moves for queen from (3,3) " [(3,4),(3,5),(3,6),(3,7),(3,8),(4,4),(5,5),(6,6),(7,7),(8,8),(4,3),(5,3),(6,3),(7,3),(8,3),(4,2),(5,1),(3,2),(3,1),(2,2),(1,1),(2,3),(1,3),(2,4),(1,5)] (possibleMoves (Piece Queen (3,3) White))

testCheckMate = TestCase $ assertEqual "checkMate test" False (isCheckMate (Piece King (6,4) Black) checkMateTestBoard)
testStaleMateTrue = TestCase $ assertEqual "stalemate test when true" True (isStaleMate staleMateTestBoard1 (Piece King (1,1) White))
testStaleMateFalse = TestCase $ assertEqual "stalemate test when false" False (isStaleMate staleMateTestBoard2 (Piece King (1,1) White))

validMoveKing = TestCase $ assertEqual "king move to check position" True (isValidMove staleMateTestBoard2 (Piece King (1,1) White) (2,1))
runtest = runTestTT $ TestList [test1,test2,test3,test4,test5,testCheckMate, testStaleMateTrue,testStaleMateFalse,validMoveKing,testPossibleMovesQueen]
estIsQueening = TestCase $ assertEqual "check if queening is True" True (isQueening (Piece Pawn (2,7) Black) (1,7))

schooltest = runTestTT $ TestList [testSchool1,testSchool2,testSchool3,testSchool4,testSchool5,testSchool6,testSchool7,testSchool8]


testBoard = [Piece Rock (1,1) White, Piece Knight (1,2) White, Piece Bishop (1,3) White, Empty (1,4),Piece King (1,5) White,Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,
                Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Piece Pawn (2,4) White,Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,
                Empty (3,1),Empty(3,2),Empty(3,3),Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Piece Queen (4,4) White,Empty(4,5),Empty(4,6),Empty(4,7),
                Empty(4,8),Empty(5,1),Piece Pawn (5,2) Black,Piece Pawn (5,3) Black,Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
                Empty(5,8),Piece Pawn (6,1) White,Piece Pawn (6,2) White,Empty(6,3),Empty(6,4),Empty(6,5),Empty(6,6),Empty(6,7),Empty(6,8),
                Piece Pawn (7,1) Black,Empty (7,2),Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Piece Pawn (7,8) Black,
                Piece Rock (8,1) Black, Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Piece Rock (8,8) Black]


testBoard2 = [Piece Rock (1,1) White, Piece Knight (1,2) White, Piece Bishop (1,3) White, Empty (1,4),Piece King (1,5) White,Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,
                Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Piece Pawn (2,4) White,Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,
                Empty (3,1),Empty(3,2),Empty(3,3),Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Piece Queen (4,4) White,Empty(4,5),Empty(4,6),Empty(4,7),
                Empty(4,8),Empty(5,1),Piece Pawn (5,2) Black,Piece Pawn (5,3) Black,Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
                Empty(5,8),Piece Pawn (6,1) White,Piece Pawn (6,2) Black,Empty(6,3),Empty(6,4),Empty(6,5),Empty(6,6),Empty(6,7),Empty(6,8),
                Piece Pawn (7,1) Black,Empty (7,2),Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Piece Pawn (7,8) Black,
                Piece Rock (8,1) Black, Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Piece Rock (8,8) Black]


testBoard3 = [Piece Rock (1,1) White, Piece Knight (1,2) White, Piece Bishop (1,3) White, Piece Queen (1,4) White,Piece King (1,5) White,Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,
                Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Piece Pawn (2,4) White,Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,
                Empty (3,1),Empty(3,2),Empty(3,3),Empty(3,4),Empty(3,5),Piece Pawn (3,6) Black,Piece Queen (4,7) White,Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Empty(4,4),Empty(4,5),Empty(4,6),Empty(3,7),
                Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
                Empty(5,8),Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Empty(6,6),Empty(6,7),Empty(6,8),
                Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Piece Pawn (7,8) Black,
                Piece Rock (8,1) Black, Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Piece Rock (8,8) Black]

schoolBoard1 = [Piece Rock (1,1) White, Piece Knight (1,2) White, Piece Bishop (1,3) White, Piece King (1,4) White,Piece Queen (1,5) White,Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,
                Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Empty (2,4),Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,
                Empty (3,1),Empty(3,2),Empty(3,3),Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Piece Pawn (4,4) White,Empty(4,5),Empty(4,6),Empty(4,7),
                Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
                Empty(5,8),Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Empty(6,6),Empty(6,7),Empty(6,8),
                Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Piece Pawn (7,8) Black,
                Piece Rock (8,1) Black, Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Piece Rock (8,8) Black]
schoolBoard2 = [Piece Rock (1,1) White, Piece Knight (1,2) White, Piece Bishop (1,3) White, Piece King (1,4) White,Piece Queen (1,5) White,Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,
                Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Empty (2,4),Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,
                Empty (3,1),Empty(3,2),Empty(3,3),Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Piece Pawn (4,4) White,Empty(4,5),Empty(4,6),Empty(4,7),
                Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
               Piece Pawn (5,8) Black,Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Empty(6,6),Empty(6,7),Empty(6,8),
                Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Empty(7,8),
                Piece Rock (8,1) Black, Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Piece Rock (8,8) Black]
schoolBoard3 = [Piece Rock (1,1) White, Piece Knight (1,2) White, Empty (1,3), Piece King (1,4) White,Piece Queen (1,5) White,Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,
                Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Empty (2,4),Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,
                Empty (3,1),Empty(3,2),Empty(3,3),Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Piece Pawn (4,4) White,Empty(4,5),Piece Bishop (4,6) White,Empty(4,7),
                Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
               Piece Pawn (5,8) Black,Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Empty(6,6),Empty(6,7),Empty(6,8),
                Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Empty(7,8),
                Piece Rock (8,1) Black, Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Piece Rock (8,8) Black]
schoolBoard4 = [Piece Rock (1,1) White, Piece Knight (1,2) White, Empty (1,3), Piece King (1,4) White,Piece Queen (1,5) White,Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White, Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Empty (2,4),Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White, Empty (3,1),Empty(3,2),Empty(3,3),Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8), Empty(4,1),Empty(4,2),Empty(4,3),Piece Pawn (4,4) White,Empty(4,5),Piece Bishop (4,6) White,Empty(4,7), Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7), Piece Pawn (5,8) Black,Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Empty(6,6),Empty(6,7),Piece Rock (6,8) Black,Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Empty(7,8),Piece Rock (8,1) Black, Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Empty(8,8)]
schoolBoard5 = [Piece Rock (1,1) White, Piece Knight (1,2) White, Empty (1,3), Piece King (1,4) White,Empty (1,5),Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,
                Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Empty (2,4),Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,
                Empty (3,1),Empty(3,2),Piece Queen (3,3) White,Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Piece Pawn (4,4) White,Empty(4,5),Piece Bishop (4,6) White,Empty(4,7),
                Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
               Piece Pawn (5,8) Black,Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Empty(6,6),Empty(6,7),Piece Rock (6,8) Black,
                Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Empty(7,8),
                Piece Rock (8,1) Black, Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Empty(8,8)]
schoolBoard6 = [Piece Rock (1,1) White, Piece Knight (1,2) White, Empty (1,3), Piece King (1,4) White,Empty (1,5),Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,
                Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Empty (2,4),Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,
                Empty (3,1),Empty(3,2),Piece Queen (3,3) White,Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Piece Pawn (4,4) White,Empty(4,5),Piece Bishop (4,6) White,Empty(4,7),
                Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
               Piece Pawn (5,8) Black,Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Piece Knight (6,6) Black,Empty(6,7),Piece Rock (6,8) Black,
                Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Empty(7,8),
                Piece Rock (8,1) Black, Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black, Empty (8,7),Empty(8,8)]
schoolBoard7 = [Piece Rock (1,1) White, Piece Knight (1,2) White, Empty (1,3), Piece King (1,4) White,Empty (1,5),Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,
                Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Empty (2,4),Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,
                Empty (3,1),Empty(3,2),Empty (3,3),Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Piece Pawn (4,4) White,Empty(4,5),Piece Bishop (4,6) White,Empty(4,7),
                Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
               Piece Pawn (5,8) Black,Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Piece Knight (6,6) Black,Empty(6,7),Piece Rock (6,8) Black,
                Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Queen (7,3) White,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Empty(7,8),
                Piece Rock (8,1) Black, Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black, Empty (8,7),Empty(8,8)]
schoolBoard8 = [Piece Rock (1,1) White, Piece Knight (1,2) White, Empty (1,3), Piece King (1,4) White,Empty (1,5),Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,
                Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Empty (2,4),Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,
                Empty (3,1),Empty(3,2),Empty (3,3),Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Piece Pawn (4,4) White,Empty(4,5),Piece Bishop (4,6) White,Empty(4,7),
                Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
               Piece Pawn (5,8) Black,Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Piece Knight (6,6) Black,Empty(6,7),Piece Rock (6,8) Black,
                Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece King (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Empty(7,8),
                Piece Rock (8,1) Black, Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Empty (8,4) ,Piece Queen (8,5) Black,Piece Bishop (8,6) Black, Empty (8,7),Empty(8,8)]
checkMateTestBoard =  [Piece Rock (1,1) White, Piece Knight (1,2) White, Piece Bishop (1,3) White, Piece King (1,4) White,Empty (1,5),Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,
                Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Piece Pawn (2,4) White,Empty(2,5),Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,
                Empty (3,1),Empty(3,2),Empty(3,3),Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Empty(4,4),Piece Pawn (4,5) White,Empty(4,6),Empty(4,7),
                Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Piece Queen (5,4) White,Empty(5,5),Empty(5,6),Empty(5,7),
                Empty(5,8),Empty(6,1),Empty(6,2),Empty(6,3),Piece King (6,4) Black,Piece Bishop (6,5) Black,Empty(6,6),Empty(6,7),Empty(6,8),
                Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Piece Pawn (7,8) Black,
                Piece Rock (8,1) Black, Piece Knight (8,2) Black, Empty (8,3), Empty (8,4) ,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Piece Rock (8,8) Black]
staleMateTestBoard1 = [Piece King (1,1) White, Empty (1,2), Empty(1,3) ,Empty (1,4) ,Empty (1,5) ,Empty(1,6) ,Empty(1,7) ,Empty (1,8),
                Empty (2,1) ,Piece Rock (2,2) Black ,Empty(2,3) ,Empty(2,4) ,Empty(2,5),Empty(2,6) ,Empty(2,7) ,Empty(2,8) ,
                Empty (3,1),Piece Rock (3,2) Black,Empty(3,3),Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Empty(4,4),Empty(4,5),Empty(4,6),Empty(4,7),
                Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
                Empty(5,8),Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Empty(6,6),Empty(6,7),Empty(6,8),
                Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Piece Pawn (7,8) Black,
                Empty (8,1), Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Empty (8,8)]

staleMateTestBoard2 = [Piece King (1,1) White, Empty (1,2), Empty(1,3) ,Empty (1,4) ,Empty (1,5) ,Empty(1,6) ,Empty(1,7) ,Empty (1,8),
                Empty (2,1) ,Piece Rock (2,2) Black ,Empty(2,3) ,Empty(2,4) ,Empty(2,5),Empty(2,6) ,Empty(2,7) ,Empty(2,8) ,
                Empty (3,1),Empty (3,2) ,Empty(3,3),Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Empty(4,4),Empty(4,5),Empty(4,6),Empty(4,7),
                Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
                Empty(5,8),Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Empty(6,6),Empty(6,7),Empty(6,8),
                Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Piece Pawn (7,8) Black,
                Empty (8,1), Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Empty (8,8)]
isQueeningTestBoard = [Piece King (1,1) White, Empty (1,2), Empty(1,3) ,Empty (1,4) ,Empty (1,5) ,Empty(1,6) ,Empty(1,7) ,Empty (1,8),
                Empty (2,1) ,Piece Rock (2,2) Black ,Empty(2,3) ,Empty(2,4) ,Empty(2,5),Empty(2,6) ,Piece Pawn (2,7) Black ,Empty(2,8) ,
                Empty (3,1),Empty (3,2) ,Empty(3,3),Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Empty(4,4),Empty(4,5),Empty(4,6),Empty(4,7),
                Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
                Empty(5,8),Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Empty(6,6),Empty(6,7),Empty(6,8),
                Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Empty (7,7),Piece Pawn (7,8) Black,
                Empty (8,1), Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Empty (8,8)]