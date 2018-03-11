module Main where

import Test.HUnit

import Monads

testMoveLeftOutOfBoard :: Test
testMoveLeftOutOfBoard = TestCase (assertEqual "move :: left out of board" Nothing (move LEFT (Robot 0 4)))

testMoveLeftNormal :: Test
testMoveLeftNormal = TestCase (assertEqual "move :: left normal" (Just (Robot 0 2)) (move LEFT (Robot 1 2)))

testMoveRightOutOfBoard :: Test
testMoveRightOutOfBoard = TestCase (assertEqual "move :: right out of board" Nothing (move RIGHT (Robot 5 5)))

testMoveRightNormal :: Test
testMoveRightNormal = TestCase (assertEqual "move :: right normal" (Just (Robot 5 4)) (move RIGHT (Robot 4 4)))

testMoveUpOutOfBoard :: Test
testMoveUpOutOfBoard = TestCase (assertEqual "move :: up out of board" Nothing (move UP (Robot 5 5)))

testMoveUpNormal :: Test
testMoveUpNormal = TestCase (assertEqual "move :: up normal" (Just (Robot 4 5)) (move UP (Robot 4 4)))

testMoveDownOutOfBoard :: Test
testMoveDownOutOfBoard = TestCase (assertEqual "move :: down out of board" Nothing (move DOWN (Robot 4 0)))

testMoveDownNormal :: Test
testMoveDownNormal = TestCase (assertEqual "move :: down normal" (Just (Robot 4 0)) (move DOWN (Robot 4 1)))

testsMove :: Test
testsMove = TestList [testMoveLeftOutOfBoard, testMoveLeftNormal,
                    testMoveRightOutOfBoard, testMoveRightNormal,
                    testMoveUpOutOfBoard, testMoveUpNormal,
                    testMoveDownOutOfBoard, testMoveDownNormal]

testCircleBoard :: Test
testCircleBoard = TestCase (assertEqual "applyMoves :: circle board" (Just (Robot 0 0)) (applyMoves (Robot 0 0) [UP, UP, UP, UP, UP, RIGHT, RIGHT, RIGHT, RIGHT, RIGHT, DOWN, DOWN, DOWN, DOWN, DOWN, LEFT, LEFT, LEFT ,LEFT, LEFT]))

testCircleBoardOutLastTry :: Test
testCircleBoardOutLastTry = TestCase (assertEqual "applyMoves :: out last try" Nothing (applyMoves (Robot 0 0) [UP, UP, UP, UP, UP, RIGHT, RIGHT, UP]))

testCircleBoardOutKeepTrying :: Test
testCircleBoardOutKeepTrying = TestCase (assertEqual "applyMoves :: out keep trying" Nothing (applyMoves (Robot 0 0) [UP, UP, UP, LEFT, RIGHT, UP, UP, RIGHT, RIGHT, RIGHT]))

testsApplyMoves :: Test
testsApplyMoves = TestList [testCircleBoard, testCircleBoardOutLastTry, testCircleBoardOutKeepTrying]

main :: IO Counts
main = do _ <- runTestTT testsMove
          runTestTT testsApplyMoves
