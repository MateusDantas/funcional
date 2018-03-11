module Main where

import Test.HUnit

import Functor

testNilTree :: Test
testNilTree = TestCase (assertEqual "nil tree" NIL (multiplyByTwo NIL))

testFullTree :: Test
testFullTree = TestCase (assertEqual "full tree" (Node 1 (Node 4 (Node 6 NIL NIL) (Node 8 NIL NIL)) (Node 10 NIL NIL)) (multiplyByTwo (Node 1 (Node 2 (Node 3 NIL NIL) (Node 4 NIL NIL)) (Node 5 NIL NIL))))

testsMultiplyByTwo :: Test
testsMultiplyByTwo = TestList [testNilTree, testFullTree]

main :: IO Counts
main = runTestTT testsMultiplyByTwo
