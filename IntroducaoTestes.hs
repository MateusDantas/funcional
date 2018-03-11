module Main where

import Test.HUnit

import Introducao

testZEquals0 :: Test
testZEquals0 = TestCase (assertEqual "sumDivisor :: z == 0" 0 (sumDivisor 1 2 0))

testZEquals1 :: Test
testZEquals1 = TestCase (assertEqual "sumDivisor :: z == 1" 5 (sumDivisor 1 5 1))

testXGreaterThanY :: Test
testXGreaterThanY = TestCase (assertEqual "sumDivisor :: x > y" 0 (sumDivisor 3 2 0))

testDivisorAtEdges :: Test
testDivisorAtEdges = TestCase (assertEqual "sumDivisor :: divisor at edge" 3 (sumDivisor 3 9 3))

testNoDivisor :: Test
testNoDivisor = TestCase (assertEqual "sumDivisor :: no divisor" 0 (sumDivisor 5 8 9))

testPrimeNumber :: Test
testPrimeNumber = TestCase (assertEqual "sumDivisor :: prime z" 14 (sumDivisor 6 99 7))

testsSumDivisor :: Test
testsSumDivisor = TestList
  [testZEquals0, testZEquals1, testXGreaterThanY, testDivisorAtEdges, testNoDivisor, testPrimeNumber]

testNEquals1 :: Test
testNEquals1 = TestCase (assertEqual "collatzSteps :: n equals 1" 1 (collatzSteps 1))

testPowerOf2N :: Test
testPowerOf2N = TestCase (assertEqual "collatzSteps :: power of 2 n" 8 (collatzSteps 128))

testRandomN :: Test
testRandomN = TestCase (assertEqual "collatzSteps :: random n" 42 (collatzSteps 139))

testsCollatzSteps :: Test
testsCollatzSteps = TestList [testNEquals1, testPowerOf2N, testRandomN]

testEmptyArray :: Test
testEmptyArray = TestCase (assertEqual "reverseArray :: empty array" ([] :: [Int]) (reverseArray []))

testArraySize1 :: Test
testArraySize1 = TestCase (assertEqual "reverseArray :: array size 1" ([1] :: [Int]) (reverseArray [1]))

testRandomArray :: Test
testRandomArray = TestCase (assertEqual "reverseArray :: random array" ([5, 3, 0, 1, 2] :: [Int]) (reverseArray [2, 1, 0, 3, 5]))

testsReverseArray :: Test
testsReverseArray = TestList [testEmptyArray, testArraySize1, testRandomArray]

main :: IO Counts
main = do _ <- runTestTT testsSumDivisor
          runTestTT testsCollatzSteps
          runTestTT testsReverseArray
