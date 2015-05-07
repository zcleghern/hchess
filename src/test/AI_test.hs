module AI_test where

import Test.HUnit
import AI
import GameLogic
import ChessBoard

main = do runTestTT $ TestList [t1]
        
t1 = TestCase $ assertEqual "value of pawn is 1" (value (Just (Pawn White))) 1