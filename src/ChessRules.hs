module ChessRules where

import ChessBoard

--attacking :: Board -> Coord -> Coord -> Bool
--attacking b c1 c2 = 

pawnAtk :: Board -> Coord -> Coord -> Bool
pawnAtk b c@(Coord x1 y1) (Coord x2 y2)
        | color (getP b c) == White = (x1 == x2 + 1 || x1 == x2 - 1) && y1 == y2 - 1
        | otherwise = (x1 == x2 + 1 || x1 == x2 - 1) && y1 == y2 + 1
        
kingAtk :: Board -> Coord -> Coord -> Bool
kingAtk b c1 c2 = adj c1 c2 || diag c1 c2

rookAtk :: Board -> Coord -> Coord -> Bool
rookAtk b (Coord x1 y1) (Coord x2 y2)
        | y1 == y2 = if x1 > x2 then 
                null . filter (/= Empty) . take (x1 - x2) . drop x2 $ row b y1 else
                null . filter (/= Empty) . take (x2 - x1) . drop x1 $ row b y1
        | x1 == x2 = if y1 > y2 then
                null . filter (/= Empty) . take (y1 - y2) . drop y2 $ col b x1 else
                null . filter (/= Empty) . take (y2 - y1) . drop y1 $ col b x1

bishopAtk :: Board -> Coord -> Coord -> Bool
bishopAtk b c1 c2
        | c1 == c2 = False
        | otherwise = bishopAtkHelp b c1 c2

bishopAtkHelp :: Board -> Coord -> Coord -> Bool
bishopAtkHelp b (Coord x1 y1) (Coord x2 y2)
        | x1 < x2 && y1 < y2 && (getP b (Coord (x1+1) (y1+1)) == Empty) = bishopAtkHelp b (Coord (x1+1) (y1+1)) (Coord x2 y2)
        | x1 < x2 && y1 > y2 && (getP b (Coord (x1+1) (y1-1)) == Empty) = bishopAtkHelp b (Coord (x1+1) (y1-1)) (Coord x2 y2)
        | x1 > x2 && y1 < y2 && (getP b (Coord (x1-1) (y1+1)) == Empty) = bishopAtkHelp b (Coord (x1-1) (y1+1)) (Coord x2 y2)
        | x1 > x2 && y1 > y2 && (getP b (Coord (x1-1) (y1-1)) == Empty) = bishopAtkHelp b (Coord (x1-1) (y1-1)) (Coord x2 y2)
        | x1 == x2 && y1 == y2 = True
        | otherwise = False
                

queenAtk :: Board -> Coord -> Coord -> Bool
queenAtk b c1 c2 = rookAtk b c1 c2 || bishopAtk b c1 c2

knightAtk :: Board -> Coord -> Coord -> Bool
knightAtk b (Coord x1 y1) (Coord x2 y2) = abs (x1 - x2) == 2 && abs (y1 - y2) == 1 ||  abs (y1 - y2) == 2 && abs (x1 - x2) == 1