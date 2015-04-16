module CastlingTests where

import Test.HUnit
import Castling
import GameLogic

tests = TestList [TestLabel "h8RookMoved" test1, TestLabel "h8RookNotMoved" test2]

test1 = TestCase $ assertEqual "h8Rook has moved, h8RookCanCastle false" (h8RookCanCastle fl) False
        where fl = ["h8Rook"]
test2 = TestCase $ assertEqual "h8Rook not moved, h8RookCanCastle true" (h8RookCanCastle fl) True
        where fl = []
        
test3 = TestCase $ assertEqual "empty space between white king and h1 rook" (checkSpaces (g,mv)) True
        where g = Game (addP (White Rook) (addP (White King) initBoard (Coord 5 1)) (Coord 8 1)) []
             mv = Move (Coord 5 1) (Coord 7 1)