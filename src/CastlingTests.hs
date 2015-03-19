module CastlingTests where

import Test.HUnit
import Castling
import GameLogic
import ChessBoard
import ChessRules

tests = TestList [test1, test2, test3, test4, test5, test6]

--check flags tests

test1 = TestCase $ assertEqual "h8Rook has moved, h8RookCanCastle false" False (h8RookCanCastle fl)
        where fl = ["h8Rook"]
test2 = TestCase $ assertEqual "h8Rook not moved, h8RookCanCastle true" True (h8RookCanCastle fl)
        where fl = []
        
--checkSpaces tests        

test3 = TestCase $ assertEqual "empty space between white king and h1 rook" (Just (g,mv)) (checkSpaces (g,mv))
        where g = Game (addP (Rook White) (addP (King White) blankBoard (Coord 5 1)) (Coord 8 1)) []
              mv = Move (Coord 5 1) (Coord 7 1)
    
test4 = TestCase $ assertEqual "piece between white king and h1 rook" (Just (g,mv)) (checkSpaces (g,mv))
        where g = Game (addP (Bishop White) (addP (Rook White) (addP (King White) blankBoard (Coord 5 1)) (Coord 8 1)) (Coord 3 1)) []
              mv = Move (Coord 5 1) (Coord 7 1)
              
-- checkFlags tests

test5 = TestCase $ assertEqual "h1 rook and white king not moved" (Just (g,mv)) (checkFlags (g,mv))
        where g = Game (addP (Rook White) (addP (King White) blankBoard (Coord 5 1)) (Coord 8 1)) []
              mv = Move (Coord 5 1) (Coord 7 1)

test6 = TestCase $ assertEqual "a8 rook and black king not moved" (Just (g,mv)) (checkFlags (g,mv))
        where g = Game (addP (Rook Black) (addP (King Black) blankBoard (Coord 5 8)) (Coord 8 8)) []
              mv = Move (Coord 5 8) (Coord 7 8)   