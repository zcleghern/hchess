module Castling where

import ChessBoard
import ChessRules
import Control.Applicative

--you can assume c1 is a king and is an attempted castle
castle :: Game -> Move -> Maybe Game
castle g mv = do
        checkedFl <- checkFlags (g, mv)
        checkedSp <- checkSpaces checkedFl
        performCastle checkedSp

performCastle :: (Game, Move) -> Maybe Game
performCastle (Game b fl t, mv) = do
        kMoved <- moveP b mv
        rookMv <- castleRook mv
        rMoved <- moveP kMoved rookMv
        return $ Game rMoved (case mv of
                Move (Coord 1 _) _ -> ("wKing":fl)
                Move (Coord 8 _) _ -> ("bKing":fl)
                _ -> fl) t
        
castleRook :: Move -> Maybe Move
castleRook mv = case mv of 
        Move _ (Coord 3 1) -> Just (Move (Coord 1 1) (Coord 4 1))
        Move _ (Coord 7 1) -> Just (Move (Coord 8 1) (Coord 6 1))
        Move _ (Coord 3 8) -> Just (Move (Coord 1 8) (Coord 4 8))
        Move _ (Coord 7 8) -> Just (Move (Coord 1 8) (Coord 6 8))
        _ -> Nothing
        
checkFlags :: (Game, Move) -> Maybe (Game, Move)
checkFlags (g@(Game b fl _), mv@(Move c1 c2)) = case mv of
        Move (Coord 5 1) (Coord 3 1) -> if getP b c1 == Just (King White) && notElem "a1Rook" fl && notElem "wKing" fl then Just (g, mv) else Nothing
        Move (Coord 5 1) (Coord 7 1) -> if getP b c1 == Just (King White) && notElem "h1Rook" fl && notElem "wKing" fl then Just (g, mv) else Nothing
        Move (Coord 5 8) (Coord 3 8) -> if getP b c1 == Just (King Black) && notElem "a8Rook" fl && notElem "bKing" fl then Just (g, mv) else Nothing
        Move (Coord 5 8) (Coord 7 8) -> if getP b c1 == Just (King Black) && notElem "h8Rook" fl && notElem "bKing" fl then Just (g, mv) else Nothing
        _ -> Nothing

checkSpaces :: (Game, Move) -> Maybe (Game, Move)
checkSpaces (g@(Game b _ _), mv@(Move c1 c2)) = case mv of
        Move (Coord 5 1) (Coord 3 1) -> if getP b c1 == Just (King White) && attacking b (Coord 1 1) (Coord 4 1) && getP b (Coord 4 1) == Nothing then Just (g, mv) else Nothing
        Move (Coord 5 1) (Coord 7 1) -> if getP b c1 == Just (King White) && attacking b (Coord 8 1) (Coord 6 1) && getP b (Coord 6 1) == Nothing then Just (g, mv) else Nothing
        Move (Coord 5 8) (Coord 3 8) -> if getP b c1 == Just (King Black) && attacking b (Coord 1 8) (Coord 4 8) && getP b (Coord 4 8) == Nothing then Just (g, mv) else Nothing
        Move (Coord 5 8) (Coord 7 8) -> if getP b c1 == Just (King Black) && attacking b (Coord 8 8) (Coord 6 8) && getP b (Coord 6 8) == Nothing then Just (g, mv) else Nothing
        _ -> Nothing
