module ChessRules where

import ChessBoard


attacking :: Board -> Coord -> Coord -> Bool
attacking b c1 c2 = atkHelper (getP b c1) b c1 c2
        where atkHelper p = case p of
                Pawn _ -> pawnAtk
                King _ -> kingAtk
                Queen _ -> queenAtk
                Knight _ -> knightAtk
                Bishop _ -> bishopAtk
                Rook _ -> rookAtk
                Empty -> \b c1 c2 -> False

pawnAtk :: Board -> Coord -> Coord -> Bool
pawnAtk b c@(Coord x1 y1) (Coord x2 y2) = case color (getP b c) of
        Just White -> (x1 == x2 + 1 || x1 == x2 - 1) && y1 == y2 - 1
        Just Black -> (x1 == x2 + 1 || x1 == x2 - 1) && y1 == y2 + 1
        Nothing -> False
        
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
        | otherwise = False

bishopAtk :: Board -> Coord -> Coord -> Bool
bishopAtk b c1 c2
        | c1 `westOf` c2 && c1 `northOf` c2 = bishopAtkSE b c1 c2
        | c1 `eastOf` c2 && c1 `northOf` c2 = bishopAtkSW b c1 c2
        | c1 `westOf` c2 && c1 `southOf` c2 = bishopAtkNE b c1 c2
        | c1 `eastOf` c2 && c1 `southOf` c2 = bishopAtkNW b c1 c2
        | otherwise = False
        
bishopAtkSE :: Board -> Coord -> Coord -> Bool
bishopAtkSE b c1@(Coord x1 y1) c2@(Coord x2 y2)
        | x1+1 == x2 && y1+1 == y2 = True
        | getP b (Coord (x1+1) (y1+1)) == Empty = bishopAtkSE b (Coord (x1+1) (y1+1)) c2
        | otherwise = False
        
bishopAtkSW :: Board -> Coord -> Coord -> Bool
bishopAtkSW b c1@(Coord x1 y1) c2@(Coord x2 y2)
        | x1-1 == x2 && y1+1 == y2 = True
        | getP b (Coord (x1-1) (y1+1)) == Empty = bishopAtkSW b (Coord (x1-1) (y1+1)) c2
        | otherwise = False
        
bishopAtkNE :: Board -> Coord -> Coord -> Bool
bishopAtkNE b c1@(Coord x1 y1) c2@(Coord x2 y2)
        | x1+1 == x2 && y1-1 == y2 = True
        | getP b (Coord (x1+1) (y1-1)) == Empty = bishopAtkNE b (Coord (x1+1) (y1-1)) c2
        | otherwise = False
        
bishopAtkNW :: Board -> Coord -> Coord -> Bool
bishopAtkNW b c1@(Coord x1 y1) c2@(Coord x2 y2)
        | x1-1 == x2 && y1-1 == y2 = True
        | getP b (Coord (x1-1) (y1-1)) == Empty = bishopAtkNW b (Coord (x1-1) (y1-1)) c2
        | otherwise = False

                
queenAtk :: Board -> Coord -> Coord -> Bool
queenAtk b c1 c2 = rookAtk b c1 c2 || bishopAtk b c1 c2

knightAtk :: Board -> Coord -> Coord -> Bool
knightAtk b (Coord x1 y1) (Coord x2 y2) = (abs (x1 - x2) == 2 && abs (y1 - y2) == 1) 
                                        ||  (abs (y1 - y2) == 2 && abs (x1 - x2) == 1)