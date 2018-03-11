module Main where

import Test.HUnit

import FuncoesSintaxe

testRandomStrings1 :: Test
testRandomStrings1 = TestCase (assertEqual "initials :: random strings 1" "a b" (initials "adjsadkasj" "bdlaskkdlsakd"))

testRandomStrings2 :: Test
testRandomStrings2 = TestCase (assertEqual "initials :: random strings 2" "A f" (initials "Adjsdsaasj" "fKLLDSAKL"))

testsInitials :: Test
testsInitials = TestList [testRandomStrings1, testRandomStrings2]

testSamePoint :: Test
testSamePoint = TestCase (assertEqual "distance :: same point" 0.0 (distance (2, 2) (2, 2)))

testIntegerResult :: Test
testIntegerResult = TestCase (assertEqual "distance :: integer result" 2.0 (distance (0, 0) (0, 2)))

testRandomResult :: Test
testRandomResult = TestCase (assertEqual "distance :: random result" 36.124783736376884 (distance (1, 3) (25, 30)))

testsDistance :: Test
testsDistance = TestList [testSamePoint, testIntegerResult, testRandomResult]

testEmptyArray :: Test
testEmptyArray = TestCase (assertEqual "testEvenLength :: empty array" True (testEvenLength []))

testRandomOddArray :: Test
testRandomOddArray = TestCase (assertEqual "testEvenLength :: test random odd array" False (testEvenLength [2, 3, 1, 0, 5]))

testRandomEvenArray :: Test
testRandomEvenArray = TestCase (assertEqual "testEvenLength :: test random even array" True (testEvenLength [2, 1, 0, 99, 1, 3]))

testsTestEvenLength :: Test
testsTestEvenLength = TestList [testEmptyArray, testRandomOddArray, testRandomEvenArray]

main :: IO Counts
main = do _ <- runTestTT testsInitials
          runTestTT testsDistance
          runTestTT testsTestEvenLength
