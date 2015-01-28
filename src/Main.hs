module Main where

import ChessBoard
import ChessRules

main::IO()
main = do
        let c1 = Coord 3 5
        let c2 = Coord 4 4
        let w = addP (Pawn Black) initBoard 3 5 --removeP initBoard 2 2
        let x = addP (Pawn White) w 4 4
        putStrLn $ printBoard x
        putStrLn . show $ attacking x c1 c2
        putStrLn . show $ bishopAtk x c1 c2
        putStrLn . show $ knightAtk x c1 c2
        putStrLn . show $ rookAtk x c1 c2
        putStrLn . show $ queenAtk x c1 c2
        putStrLn . show $ kingAtk x c1 c2