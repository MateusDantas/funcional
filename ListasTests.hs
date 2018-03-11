module Main where

import Test.HUnit

import Listas

testEmptyArray :: Test
testEmptyArray = TestCase (assertEqual "flatten :: empty array" ([] :: [Int]) (flatten []))

testEmptyArrays :: Test
testEmptyArrays = TestCase (assertEqual "flatten :: empty arrays" ([] :: [Int]) (flatten [[], [], []]))

testSomeEmptyArrays :: Test
testSomeEmptyArrays = TestCase (assertEqual "flatten :: some empty arrays" [1,2,3,4] (flatten [[1,2], [], [], [3], [4]]))

testRandomArrays :: Test
testRandomArrays = TestCase (assertEqual "flatten :: random arrays" [5, 4, 2, 0, 1] (flatten [[5, 4], [], [2], [0], [1]]))

testsFlatten :: Test
testsFlatten = TestList [testEmptyArray, testEmptyArrays, testSomeEmptyArrays, testRandomArrays]

testWhileEmptyArray :: Test
testWhileEmptyArray = TestCase (assertEqual "whilePred :: empty array" ([] :: [Int]) (whilePred even []))

testWhileFullArray :: Test
testWhileFullArray = TestCase (assertEqual "whilePred :: full array" [1, 2, 3, 4] (whilePred (<5) [1, 2, 3, 4]))

testWhileNone :: Test
testWhileNone = TestCase (assertEqual "whilePred :: none" ([] :: [Int]) (whilePred (<0) [1, 2, 3, 4, 5]))

testWhileRandom :: Test
testWhileRandom = TestCase (assertEqual "whilePred :: random" [5, 7, 9, 3] (whilePred odd [5, 7, 9, 3, 2, 4, 6, 8]))

testsWhilePred :: Test
testsWhilePred = TestList [testWhileEmptyArray, testWhileFullArray, testWhileNone, testWhileRandom]

testMinListRandom1 :: Test
testMinListRandom1 = TestCase (assertEqual "minList :: random1" 2 (minList [9, 10, 4, 5, 7, 2, 3, 12]))

testMinListRandom2 :: Test
testMinListRandom2 = TestCase (assertEqual "minList :: random2" 1 (minList [9, 10, 1, 5, 7, 2, 3, 12]))

testMinListRandom3 :: Test
testMinListRandom3 = TestCase (assertEqual "minList :: random3" 20 (minList [90, 100, 40, 20, 70, 22, 30, 120]))

testsMinList :: Test
testsMinList = TestList [testMinListRandom1, testMinListRandom2, testMinListRandom3]

testMaxListRandom1 :: Test
testMaxListRandom1 = TestCase (assertEqual "maxList :: random1" 71 (maxList [9, 10, 4, 5, 71, 2, 3, 12]))

testMaxListRandom2 :: Test
testMaxListRandom2 = TestCase (assertEqual "maxList :: random2" 52 (maxList [9, 10, 1, 52, 7, 2, 3, 12]))

testMaxListRandom3 :: Test
testMaxListRandom3 = TestCase (assertEqual "maxList :: random3" 100 (maxList [90, 100, 40, 20, 70, 22, 30, 12]))

testsMaxList :: Test
testsMaxList = TestList [testMaxListRandom1, testMaxListRandom2, testMaxListRandom3]

testRemoveEmptyArray :: Test
testRemoveEmptyArray = TestCase (assertEqual "removePred :: empty array" ([] :: [Int]) (removePred even []))

testRemoveFullArray :: Test
testRemoveFullArray = TestCase (assertEqual "removePred :: full array" ([] :: [Int]) (removePred (<5) [1, 2, 3, 4]))

testRemoveNone :: Test
testRemoveNone = TestCase (assertEqual "removePred :: none" [1, 2, 3, 4, 5] (removePred (<0) [1, 2, 3, 4, 5]))

testRemoveRandom :: Test
testRemoveRandom = TestCase (assertEqual "removePred :: random" [2, 4, 6, 8] (removePred odd [5, 7, 9, 3, 2, 4, 6, 8]))

testsRemovePred :: Test
testsRemovePred = TestList [testRemoveEmptyArray, testRemoveFullArray, testRemoveNone, testRemoveRandom]

testDoubleRandom1 :: Test
testDoubleRandom1 = TestCase (assertEqual "doubleSum :: random1" 232 (doubleSum [9, 10, 4, 5, 71, 2, 3, 12]))

testDoubleRandom2 :: Test
testDoubleRandom2 = TestCase (assertEqual "doubleSum :: random2" 192 (doubleSum [9, 10, 1, 52, 7, 2, 3, 12]))

testDoubleRandom3 :: Test
testDoubleRandom3 = TestCase (assertEqual "doubleSum :: random3" 768 (doubleSum [90, 100, 40, 20, 70, 22, 30, 12]))

testsDoubleSum :: Test
testsDoubleSum = TestList [testDoubleRandom1, testDoubleRandom2, testDoubleRandom3]

main :: IO Counts
main = do _ <- runTestTT testsFlatten
          runTestTT testsWhilePred
          runTestTT testsMinList
          runTestTT testsMaxList
          runTestTT testsRemovePred
          runTestTT testsDoubleSum
