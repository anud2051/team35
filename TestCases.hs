
import Test.HUnit
import BoardStatus
import Play
import MovePiece
import PossibleMoves
import ValidMove
import Data 

test1 = TestCase $ assertEqual "nearestPieces"
            [Piece Pawn (3,4) Black,Piece Pawn (5,4) Black,Piece Pawn (4,3) Black,Piece Pawn (4,5) Black,Piece Pawn (3,3) Black,Piece Pawn (3,5) Black,Piece Pawn (5,3) Black,Piece Pawn (5,5) Black]
 (findNearestPieces (Piece Rock (4,4) White) [(Piece Pawn (2,4) Black), (Piece Pawn (3,4) Black),(Piece Pawn (1,4) Black), (Piece Pawn (6,4) Black), (Piece Pawn (5,4) Black),(Piece Pawn (8,4) Black), (Piece Pawn (4,1) Black),(Piece Pawn (4,3) Black),(Piece Pawn (4,5) Black),(Piece Pawn (4,6) Black),(Piece Pawn (3,3) Black), (Piece Pawn (2,2) Black),(Piece Pawn (5,5) Black),(Piece Pawn (7,7) Black), (Piece Pawn (8,8) Black),(Piece Pawn (6,6) Black), (Piece Pawn (2,6) Black),(Piece Pawn (3,5) Black), (Piece Pawn (1,7) Black), (Piece Pawn (6,2) Black),(Piece Pawn (5,3) Black), (Piece Pawn (7,1) Black)] )

test2 = let allSquares = posToBoard (possibleMoves (Piece Rock (4,4) Black)) initialBoard in  TestCase $ assertEqual "nearestPieces Combained With Functions" [Piece Pawn (2,4) White,Piece Pawn (7,4) Black,Empty (1,1),Empty (1,1),Empty (1,1),Empty (1,1),Empty (1,1),Empty (1,1)] (findNearestPieces (Piece Rock (4,4) Black) allSquares )
                                                                                                                                              
test3= TestCase $ assertEqual  "MovePiece" [Piece Rock (1,1) White,Piece Knight (1,2) White,Piece Bishop (1,3) White,Piece Queen (1,4) White,Piece King (1,5) White,Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Piece Pawn (2,4) White,Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,Empty (3,1),Empty (3,2),Empty (3,3),Empty (3,4),Empty (3,5),Empty (3,6),Empty (3,7),Empty (3,8),Empty (4,1),Empty (4,2),Empty (4,3),Empty (4,4),Empty (4,5),Empty (4,6),Empty (4,7),Piece Rock (4,8) Black,Empty (5,1),Empty (5,2),Empty (5,3),Empty (5,4),Empty (5,5),Empty (5,6),Empty (5,7),Empty (5,8),Empty (6,1),Empty (6,2),Empty (6,3),Empty (6,4),Empty (6,5),Empty (6,6),Empty (6,7),Empty (6,8),Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Piece Pawn (7,8) Black,Piece Rock (8,1) Black,Piece Knight (8,2) Black,Piece Bishop (8,3) Black,Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Empty (8,8)] (movePiece (Piece Rock (8,8) Black) (4,8) initialBoard)

test4= TestCase $ assertEqual "validMove" True (validMove initialBoard (Piece Pawn (2,1) White) (3,1))
test5 = TestCase $ assertEqual "PawnMoveStraight" False (validMove testBoard2 (Piece Pawn (7,1) Black) (6,1))

testSchool1 = TestCase $ assertEqual "validMove White -Pawn (2,4) to (4,4)" True (validMove initialBoard (Piece Pawn (2,4) White) (4,4))
testSchool2 = TestCase $ assertEqual "validMove Black- Pawn (7,8) to (5,8)" True (validMove schoolBoard1 (Piece Pawn (7,8) Black) (5,8))

testSchool3 = TestCase $ assertEqual "validMove White - Bishop (1,3) to (4,6)" True (validMove schoolBoard2 (Piece Bishop (1,3) White) (4,6))

testSchool4 = TestCase $ assertEqual "validMove Black- Rock (8,8) to (7,8)" True (validMove schoolBoard3 (Piece Rock (8,8) Black) (7,8))
testSchool5 = TestCase $ assertEqual "validMove White- Queen (1,5) to (3,3)" True (validMove schoolBoard4 (Piece Queen (1,5) White) (3,3))
testSchool6 = TestCase $ assertEqual "validMove Black- Knight (8,7) to (6,6)" True (validMove schoolBoard5 (Piece Knight (8,7) Black) (6,6))

testSchool7 = TestCase $ assertEqual "validMove White- Queen (3,3) to (7,3)" True (validMove schoolBoard6 (Piece Queen (3,3) White) (7,3))
testSchool8 = TestCase $ assertEqual "isCheckMate at school mate board" True (isCheckMate (Piece King (8,4) Black) schoolBoard7)

runtest = runTestTT $ TestList [test1,test2,test3,test4,test5]

schooltest = runTestTT $ TestList [testSchool1,testSchool2,testSchool3,testSchool4,testSchool5,testSchool6,testSchool7]


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
schoolBoard4 = [Piece Rock (1,1) White, Piece Knight (1,2) White, Empty (1,3), Piece King (1,4) White,Piece Queen (1,5) White,Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,
                Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Empty (2,4),Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,
                Empty (3,1),Empty(3,2),Empty(3,3),Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Piece Pawn (4,4) White,Empty(4,5),Piece Bishop (4,6) White,Empty(4,7),
                Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
               Piece Pawn (5,8) Black,Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Empty(6,6),Empty(6,7),Piece Rock (6,8) Black,
                Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Empty(7,8),
                Piece Rock (8,1) Black, Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Empty(8,8)]
schoolBoard5 = [Piece Rock (1,1) White, Piece Knight (1,2) White, Empty (1,3), Piece King (1,4) White,Empty (3,3),Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,
                Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Empty (2,4),Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,
                Empty (3,1),Empty(3,2),Piece Queen (3,3) White,Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Piece Pawn (4,4) White,Empty(4,5),Piece Bishop (4,6) White,Empty(4,7),
                Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
               Piece Pawn (5,8) Black,Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Empty(6,6),Empty(6,7),Piece Rock (6,8) Black,
                Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Empty(7,8),
                Piece Rock (8,1) Black, Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black,Piece Knight (8,7) Black,Empty(8,8)]
schoolBoard6 = [Piece Rock (1,1) White, Piece Knight (1,2) White, Empty (1,3), Piece King (1,4) White,Empty (3,3),Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,
                Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Empty (2,4),Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,
                Empty (3,1),Empty(3,2),Piece Queen (3,3) White,Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Piece Pawn (4,4) White,Empty(4,5),Piece Bishop (4,6) White,Empty(4,7),
                Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
               Piece Pawn (5,8) Black,Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Piece Knight (6,6) Black,Empty(6,7),Piece Rock (6,8) Black,
                Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Pawn (7,3) Black,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Empty(7,8),
                Piece Rock (8,1) Black, Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black, Empty (8,7),Empty(8,8)]
schoolBoard7 = [Piece Rock (1,1) White, Piece Knight (1,2) White, Empty (1,3), Piece King (1,4) White,Empty (3,3),Piece Bishop (1,6) White,Piece Knight (1,7) White,Piece Rock (1,8) White,
                Piece Pawn (2,1) White,Piece Pawn (2,2) White,Piece Pawn (2,3) White,Empty (2,4),Piece Pawn (2,5) White,Piece Pawn (2,6) White,Piece Pawn (2,7) White,Piece Pawn (2,8) White,
                Empty (3,1),Empty(3,2),Empty (3,3),Empty(3,4),Empty(3,5),Empty(3,6),Empty(3,7),Empty(3,8),
                Empty(4,1),Empty(4,2),Empty(4,3),Piece Pawn (4,4) White,Empty(4,5),Piece Bishop (4,6) White,Empty(4,7),
                Empty(4,8),Empty(5,1),Empty(5,2),Empty(5,3),Empty(5,4),Empty(5,5),Empty(5,6),Empty(5,7),
               Piece Pawn (5,8) Black,Empty(6,1),Empty(6,2),Empty(6,3),Empty(6,4),Empty(6,5),Piece Knight (6,6) Black,Empty(6,7),Piece Rock (6,8) Black,
                Piece Pawn (7,1) Black,Piece Pawn (7,2) Black,Piece Queen (7,3) White,Piece Pawn (7,4) Black,Piece Pawn (7,5) Black,Piece Pawn (7,6) Black,Piece Pawn (7,7) Black,Empty(7,8),
                Piece Rock (8,1) Black, Piece Knight (8,2) Black, Piece Bishop (8,3) Black, Piece King (8,4) Black,Piece Queen (8,5) Black,Piece Bishop (8,6) Black, Empty (8,7),Empty(8,8)]
