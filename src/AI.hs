module AI where

import ChessRules
import ChessBoard
import GameLogic
import Control.Applicative

--minimax :: Game -> Move
--minimax g = maxGame $ map (\mv -> (mmMinNode 3 (move g mv), mv)) (nextMoves g)
  --      where maxGame = snd . foldr foldingF (-40, Move (Coord 1 1) (Coord 1 1))
  --            foldingF x y = if fst x < fst y then y else x

noGame :: Game
noGame = Game blankBoard [] White

--mmMaxNode :: Int -> Maybe Game -> Int
--mmMaxNode 0 (Just (Game b fl trn)) = evalNaive' b trn
--mmMaxNode d g = maximum (map (mmMinNode (d - 1)) (nextMoves g))

--mmMinNode :: Int -> Maybe Game -> Int
--mmMinNode 0 (Just (Game b fl trn)) = evalNaive' b (notC trn)
--mmMinNode d g = minimum (map (mmMaxNode (d - 1)) (nextMoves g))


values :: Game -> [Int]
values = map (\(Game brd _ trn) -> evalNaive' brd trn) . children

printout :: Game -> [(Int, Move)]
printout g = map (\mv -> (abMinNode 5 (-100) 100 (move g mv), mv)) (nextMoves g)
        where maxGame = snd . foldr foldingF (-100, Move (Coord 1 1) (Coord 1 1))
              foldingF x y = if fst y > fst x then y else x

alphaBeta :: Game -> Move
alphaBeta g = maxGame $ map (\mv -> (abMinNode 6 (-100) 100 (move g mv), mv)) (nextMoves g)
        where maxGame = snd . foldr foldingF (-100, Move (Coord 1 1) (Coord 1 1))
              foldingF x y = if fst y > fst x then y else x
       
abMaxNode :: Int -> Int -> Int -> Maybe Game -> Int
abMaxNode 0 a b (Just g) = evalNaive'' g
abMaxNode d a b (Just g@(Game _ _ trn))
        | checkMate g trn = -1000
        | checkMate g (notC trn) = 1000
        | otherwise = abMaxHelper d (-100) a b g $ nextMoves g
        
abMaxHelper :: Int -> Int -> Int -> Int -> Game -> [Move] -> Int
abMaxHelper d v a b g [] = v
abMaxHelper d v a b g (x:xs)
        | b <= newA = newV
        | otherwise = abMaxHelper d newV newA b g xs
                where newV = max v (abMinNode (d - 1) a b (move g x))
                      newA = max a newV
                     
abMinNode :: Int -> Int -> Int -> Maybe Game -> Int
abMinNode 0 a b (Just g) = evalNaive'' g
abMinNode d a b (Just g@(Game _ _ trn))
        | checkMate g trn = -1000
        | checkMate g (notC trn) = 1000 
        | otherwise = abMinHelper d 100 a b g $ nextMoves g 

abMinHelper :: Int -> Int -> Int -> Int -> Game -> [Move] -> Int
abMinHelper d v a b g [] = v
abMinHelper d v a b g (x:xs)
        | newB <= a = newV
        | otherwise = abMinHelper d newV a newB g xs
                where newV = min v (abMaxNode (d - 1) a b (move g x))
                      newB = min b newV
                      

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
                        Just clr' -> if clr == clr' then value (getP b (Coord x y)) + total2 else (value $ getP b (Coord x y)) * (-1) + total2
                        Nothing -> total2))
                        
evalNaive'' :: Game -> Int
evalNaive'' g@(Game b _ clr)
        | checkMate g clr = -1000
        | checkMate g (notC clr) = 1000
        | otherwise = loop 0 [1..8] (\total x -> 
                loop total [1..8] (\total2 y ->
                        case color <$> getP b (Coord x y) of
                                Just clr' -> if clr == clr' then value (getP b (Coord x y)) + total2 else (value $ getP b (Coord x y)) * (-1) + total2
                                Nothing -> total2))

value :: Maybe Piece -> Int
value (Just (Pawn _)) = 1
value (Just (Knight _)) = 3
value (Just (Bishop _)) = 3
value (Just (Rook _)) = 5
value (Just (Queen _)) = 9
value Nothing = 0
value _ = 0 --yes, this includes the King, whose true value would be infinity