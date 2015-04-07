module AI where

import ChessRules
import ChessBoard
import GameLogic
import Control.Applicative
import Data.Maybe

minimax :: Game -> Game
minimax g = maxGame $ map (\gm -> (mmMinNode 2 gm, gm)) (children g)
        where maxGame = snd . foldr foldingF (-40, noGame)
              foldingF x y = if fst x < fst y then y else x

noGame :: Game
noGame = Game blankBoard [] White

mmMaxNode :: Int -> Game -> Int
mmMaxNode 0 (Game b fl trn) = evalNaive' b trn
mmMaxNode d g = maximum (map (mmMinNode (d - 1)) (children g))

mmMinNode :: Int -> Game -> Int
mmMinNode 0 (Game b fl trn) = evalNaive' b (notC trn)
mmMinNode d g = minimum (map (mmMaxNode (d - 1)) (children g))

children :: Game -> [Game]
children g = filterJusts . map (move g) $ concatMap (possibleMoves g) allSpaces
        where filterJusts = foldr (\thr lst -> case thr of
                 Just g -> g:lst
                 _ -> lst) []

evalNaive :: Board -> Color -> Int
evalNaive b clr = loop 0 [1..8] (\total x -> 
        loop total [1..8] (\total2 y ->
                case color <$> getP b (Coord x y) of
                        Just clr' -> if clr == clr' then total2 + 1 else total2 - 1
                        Nothing -> total2))

evalNaive' :: Board -> Color -> Int
evalNaive' b clr = loop 0 [1..8] (\total x -> 
        loop total [1..8] (\total2 y ->
                case color <$> getP b (Coord x y) of
                        Just clr' -> if clr == clr' then value (getP b (Coord x y)) + total2 else value (getP b (Coord x y)) + (total2 * (-1))
                        Nothing -> total2))

value :: Maybe Piece -> Int
value (Just (Pawn _)) = 1
value (Just (Knight _)) = 3
value (Just (Bishop _)) = 3
value (Just (Rook _)) = 5
value (Just (Queen _)) = 9
value Nothing = 0
value _ = 0 --yes, this includes the King, whose true value would be infinity