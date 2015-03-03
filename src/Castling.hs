module Castling where

import ChessBoard
import ChessRules
import qualified Data.Map as M

castle :: Game -> Move -> (Game, Move)
castle g@(Game b fl) mv = do
        checkedFl <- checkFlags (g, mv)
        checkedSp <- checkSpaces checkedFl
        return $ (updateFlags checkedSp)


updateFlags :: (Game, Move) -> (Game, Move)
updateFlags ((Game b fl), mv@(Move c1 c2))
        | c1 == Coord 1 1 && getP b c1 == Rook White = (Game b (M.insert "a1Rook" False fl), mv)
        | c1 == Coord 8 1 && getP b c1 == Rook White = (Game b (M.insert "h1Rook" False fl), mv)
        | c1 == Coord 1 8 && getP b c1 == Rook Black = (Game b (M.insert "a8Rook" False fl), mv)
        | c1 == Coord 8 8 && getP b c1 == Rook Black = (Game b (M.insert "a1Rook" False fl), mv)
        | getP b c1 == King White = (Game b (M.insert "whiteKing" False fl), mv)
        | getP b c1 == King Black = (Game b (M.insert "blackKing" False fl), mv)
        | otherwise = (Game b fl, mv)
        
checkFlags :: (Game, Move) -> Maybe (Game, Move)
checkFlags (g@(Game b fl), mv@(Move c1 c2))
        | c1 == Coord 5 1 && c2 == Coord 3 1 && getP b c1 == King White = if a1RookCanCastle fl && safeLookup "whiteKing" fl then Just (g, mv) else Nothing
        | c1 == Coord 5 1 && c2 == Coord 7 1 && getP b c1 == King White = if h1RookCanCastle fl && safeLookup "whiteKing" fl then Just (g, mv) else Nothing
        | c1 == Coord 5 8 && c2 == Coord 3 8 && getP b c1 == King Black = if a8RookCanCastle fl && safeLookup "blackKing" fl then Just (g, mv) else Nothing
        | c1 == Coord 5 8 && c2 == Coord 7 8 && getP b c1 == King Black = if h8RookCanCastle fl && safeLookup "blackKing" fl then Just (g, mv) else Nothing
        | otherwise = Just (g,mv)

checkSpaces :: (Game, Move) -> Maybe (Game, Move)
checkSpaces (g@(Game b fl), mv@(Move c1 c2))
        | c1 == Coord 5 1 && c2 == Coord 3 1 && getP b c1 == King White = if attacking b (Coord 1 1) (Coord 4 1) && getP b (Coord 4 1) == Empty then Just (g, mv) else Nothing
        | c1 == Coord 5 1 && c2 == Coord 7 1 && getP b c1 == King White = if attacking b (Coord 8 1) (Coord 6 1) && getP b (Coord 6 1) == Empty then Just (g, mv) else Nothing
        | c1 == Coord 5 8 && c2 == Coord 3 8 && getP b c1 == King Black = if attacking b (Coord 1 8) (Coord 4 8) && getP b (Coord 4 8) == Empty then Just (g, mv) else Nothing
        | c1 == Coord 5 8 && c2 == Coord 7 8 && getP b c1 == King Black = if attacking b (Coord 8 8) (Coord 6 8) && getP b (Coord 6 8) == Empty then Just (g, mv) else Nothing
        | otherwise = Just (g,mv)

safeLookup :: String -> Flags -> Bool
safeLookup key fl = case M.lookup key fl of
        Just x -> x
        Nothing -> True
        
a1RookCanCastle :: Flags -> Bool
a1RookCanCastle = safeLookup "a1Rook"

h1RookCanCastle :: Flags -> Bool
h1RookCanCastle = safeLookup "h1Rook"

a8RookCanCastle :: Flags -> Bool
a8RookCanCastle = safeLookup "a8Rook"

h8RookCanCastle :: Flags -> Bool
h8RookCanCastle = safeLookup "h8Rook"