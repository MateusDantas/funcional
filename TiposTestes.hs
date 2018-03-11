module Main where

import Test.HUnit

import Tipos

testsRandomCircle1 :: Test
testsRandomCircle1 = TestCase (assertEqual "random circle1" 153.93805 (area (createCircle 2.0 5.0 7.0)))

testsRandomCircle2 :: Test
testsRandomCircle2 = TestCase (assertEqual "random circle2" 19.634954 (area (createCircle 0.0 0.0 2.5)))

testsRandomRectangle1 :: Test
testsRandomRectangle1 = TestCase (assertEqual "random rectangle1" 3930.6602 (area (createRectangle 2.0 3.1 54.2 78.4)))

testsRandomRectangle2 :: Test
testsRandomRectangle2 = TestCase (assertEqual "random rectangle2" 365.56 (area (createRectangle 20.0 32.1 5.2 7.4)))

testsRandomRectangle3 :: Test
testsRandomRectangle3 = TestCase (assertEqual "random rectangle3" 566.84 (area (createRectangle 20.0 32.1 5.2 70.4)))

testsArea :: Test
testsArea = TestList [testsRandomCircle1, testsRandomCircle2, testsRandomRectangle1, testsRandomRectangle2, testsRandomRectangle3]

main :: IO Counts
main = runTestTT testsArea
