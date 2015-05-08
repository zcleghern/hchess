module AI where

import ChessRules
import ChessBoard
import GameLogic
import Control.Applicative

--minimax :: Game -> Move
--minimax g = maxGame $ map (\mv -> (mmMinNode 3 (move g mv), mv)) (nextMoves g)
  --      where maxGame = snd . foldr foldingF (-40, Move (Coord 1 1) (Coord 1 1))
    --          foldingF x y = if fst x < fst y then y else x

--mmMaxNode :: Int -> Maybe Game -> Int
--mmMaxNode 0 (Just (Game b fl trn)) = evalNaive b trn
--mmMaxNode d g = maximum (map (mmMinNode (d - 1)) (nextMoves g))

--mmMinNode :: Int -> Maybe Game -> Int
--mmMinNode 0 (Just (Game b fl trn)) = evalNaive b (notC trn)
--mmMinNode d g = minimum (map (mmMaxNode (d - 1)) (nextMoves g))

alphaBeta :: Game -> Move
alphaBeta g = maxGame $ map (\mv -> (abNode 6 (-100) 100 (move g mv) False, mv)) (nextMoves g)
        where maxGame = snd . foldr foldingF (-100, Move (Coord 1 1) (Coord 1 1))
              foldingF x y = if fst y > fst x then y else x

abNode :: Int -> Int -> Int -> Maybe Game -> Bool -> Int
abNode 0 _ _ (Just g) _ = evalNaive g
abNode d a b (Just g@(Game _ _ trn)) isMax
        | checkMate g trn = -1000
        | checkMate g (notC trn) = 1000
        | otherwise = abHelper d 100 a b g (nextMoves g) isMax
abNode _ _ _ Nothing _ = 0

        
abHelper :: Int -> Int -> Int -> Int -> Game -> [Move] -> Bool -> Int
abHelper _ v _ _ _ [] _ = v
abHelper d v a b g (x:xs) isMax
        | b <= newA = newV
        | otherwise = abHelper d newV newA b g xs isMax
                where newV = max v (abNode (d - 1) a b (move g x) (not isMax))
                      newA = max a newV
                      
nextMoves :: Game -> [Move]
nextMoves g = filterLegal $ concatMap (possibleMoves g) allSpaces
        where filterLegal = foldr (\cur lst -> case move g cur of
                Nothing -> lst
                _ -> cur:lst) []

children :: Game -> [Game]
children g = filterJusts . map (move g) $ concatMap (possibleMoves g) allSpaces
        where filterJusts = foldr (\cur lst -> case cur of
                 Just gm -> gm:lst
                 _ -> lst) []
                        
evalNaive :: Game -> Int
evalNaive g@(Game b _ clr)
        | checkMate g clr = -1000
        | checkMate g (notC clr) = 1000
        | otherwise = let values p = value p * (if (color <$> p) == Just clr then 1 else -1) in
                sum $ map values [getP b (Coord x y) | x <- [1..8], y <- [1..8]]

value :: Maybe Piece -> Int
value (Just (Pawn _)) = 1
value (Just (Knight _)) = 3
value (Just (Bishop _)) = 3
value (Just (Rook _)) = 5
value (Just (Queen _)) = 9
value Nothing = 0
value _ = 0 --yes, this includes the King, whose true value would be infinity