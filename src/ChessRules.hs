module ChessRules where

import ChessBoard
import Control.Applicative

data Game = Game Board Flags Color deriving (Eq)
type Flags = [String]

instance Show Game where
        show (Game b fl t) = printBoard b ++ "\n" ++ show fl ++ "\n" ++ show t

inCheck :: Board -> Color -> Bool
inCheck b c = or . map ($ getKing b c) . map (attacking b) . filter (isOpposite b c) $ allSpaces
                        
attacking :: Board -> Coord -> Coord -> Bool
attacking b c1 c2 = atkHelper (getP b c1) b c1 c2
        where atkHelper p = case p of
                Just (Pawn _) -> pawnAtk
                Just (King _) -> kingAtk
                Just (Queen _) -> queenAtk
                Just (Knight _) -> knightAtk
                Just (Bishop _) -> bishopAtk
                Just (Rook _) -> rookAtk
                Nothing -> \_ _ _ -> False

pawnAtk :: Board -> Coord -> Coord -> Bool
pawnAtk b c@(Coord x1 y1) (Coord x2 y2) = case color <$> getP b c of
        Just White -> (x1 == x2 + 1 || x1 == x2 - 1) && y1 == y2 - 1
        Just Black -> (x1 == x2 + 1 || x1 == x2 - 1) && y1 == y2 + 1
        Nothing -> False
        
kingAtk :: Board -> Coord -> Coord -> Bool
kingAtk _ c1 c2 = adj c1 c2 || diag c1 c2

rookAtk :: Board -> Coord -> Coord -> Bool
rookAtk b (Coord x1 y1) (Coord x2 y2)
        | y1 == y2 = if x1 > x2 then 
                rookCheckLast . take (x1 - x2) . drop x2 $ row b y1 else
                rookCheckLast . take (x2 - x1) . drop x1 $ row b y1
        | x1 == x2 = if y1 > y2 then
                rookCheckLast . take (y1 - y2) . drop y2 $ col b x1 else
                rookCheckLast . take (y2 - y1) . drop y1 $ col b x1
        | otherwise = False
        
rookCheckLast :: [Maybe Piece] -> Bool
rookCheckLast [] = False
rookCheckLast [x] = True
rookCheckLast (x:xs) = if x == Nothing then rookCheckLast xs else False

bishopAtk :: Board -> Coord -> Coord -> Bool
bishopAtk b c1 c2
        | outOfBounds c1 || outOfBounds c2 = False
        | c2 `westOf` c1 && c2 `northOf` c1 = bishopAtkNW b c1 c2
        | c2 `eastOf` c1 && c2 `northOf` c1 = bishopAtkNE b c1 c2
        | c2 `westOf` c1 && c2 `southOf` c1 = bishopAtkSW b c1 c2
        | c2 `eastOf` c1 && c2 `southOf` c1 = bishopAtkSE b c1 c2
        | otherwise = False
        
bishopAtkSE :: Board -> Coord -> Coord -> Bool
bishopAtkSE b c1@(Coord x1 y1) c2@(Coord x2 y2)
        | outOfBounds c1 || outOfBounds c2 = False
        | x1+1 == x2 && y1+1 == y2 = True
        | not (outOfBounds (Coord (x1+1) (y1+1))) && getP b (Coord (x1+1) (y1+1)) == Nothing = bishopAtkSE b (Coord (x1+1) (y1+1)) c2
        | otherwise = False
        
bishopAtkSW :: Board -> Coord -> Coord -> Bool
bishopAtkSW b c1@(Coord x1 y1) c2@(Coord x2 y2)
        | outOfBounds c1 || outOfBounds c2 = False
        | x1-1 == x2 && y1+1 == y2 = True
        | not (outOfBounds (Coord (x1-1) (y1+1))) && getP b (Coord (x1-1) (y1+1)) == Nothing = bishopAtkSW b (Coord (x1-1) (y1+1)) c2
        | otherwise = False
        
bishopAtkNE :: Board -> Coord -> Coord -> Bool
bishopAtkNE b c1@(Coord x1 y1) c2@(Coord x2 y2)
        | outOfBounds c1 || outOfBounds c2 = False
        | x1+1 == x2 && y1-1 == y2 = True
        | not (outOfBounds (Coord (x1+1) (y1-1))) && getP b (Coord (x1+1) (y1-1)) == Nothing = bishopAtkNE b (Coord (x1+1) (y1-1)) c2
        | otherwise = False
        
bishopAtkNW :: Board -> Coord -> Coord -> Bool
bishopAtkNW b c1@(Coord x1 y1) c2@(Coord x2 y2)
        | outOfBounds c1 || outOfBounds c2 = False
        | x1-1 == x2 && y1-1 == y2 = True
        | not (outOfBounds (Coord (x1-1) (y1-1))) && getP b (Coord (x1-1) (y1-1)) == Nothing = bishopAtkNW b (Coord (x1-1) (y1-1)) c2
        | otherwise = False

                
queenAtk :: Board -> Coord -> Coord -> Bool
queenAtk b c1 c2 = rookAtk b c1 c2 || bishopAtk b c1 c2

knightAtk :: Board -> Coord -> Coord -> Bool
knightAtk b (Coord x1 y1) (Coord x2 y2) = (abs (x1 - x2) == 2 && abs (y1 - y2) == 1) 
                                        ||  (abs (y1 - y2) == 2 && abs (x1 - x2) == 1)