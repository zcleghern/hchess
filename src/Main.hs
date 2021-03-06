module Main where

import ChessBoard
import ChessRules
import GameLogic
import AI
import Data.Char
import Data.Maybe
import Control.Applicative

main::IO()
main = do
        playTest $ Game initBoard [] White
        return ()
        
playTest :: Game -> IO ()
playTest g@(Game b _ t) = do

        putStrLn $ printBoard b ++ "\n" ++ show t ++ " to play"
        putStrLn "Enter your move"
        input <- getLine
        let maybeResult = execute g input
        let result = fromMaybe g maybeResult
        let aiMove = alphaBeta result
        putStrLn $ "move values: " ++ printMoveValues (nextMoves result) result
        let aiResult =  case move result aiMove of
                Just gm -> gm
                _ -> Game initBoard [] White
        case status aiResult of
                WhiteMated -> putStrLn "White has been checkmated. \n Black wins!"
                BlackMated -> putStrLn "Black has been checkmated. \n White wins!"
                _ -> playTest aiResult
        
play :: Game -> IO ()
play g@(Game b _ t) = do

        putStrLn $ printBoard b ++ "\n" ++ show t ++ " to play"
        putStrLn "Enter your move"
        input <- getLine
        let maybeResult = execute g input
        let result = fromMaybe g maybeResult
        let aiMove = alphaBeta result
        let aiResult =  case move result aiMove of
                Just gm -> gm
                _ -> Game initBoard [] White
        case status aiResult of
                WhiteMated -> putStrLn "White has been checkmated. \n Black wins!"
                BlackMated -> putStrLn "Black has been checkmated. \n White wins!"
                _ -> play aiResult

execute :: Game -> String -> Maybe Game
execute g str = parseInput str >>= \mv -> move g mv   
        
parseInput :: String -> Maybe Move
parseInput [w,x,' ',y,z] = Move <$> parseCoord [w,x] <*> parseCoord [y,z]
parseInput [w,x,y,z] = Move <$> parseCoord [w,x] <*> parseCoord [y,z]
parseInput _ = Nothing

parseCoord :: String -> Maybe Coord        
parseCoord [x,y] = case toLower x of
        'a' -> Just $ Coord 1 (digitToInt y)
        'b' -> Just $ Coord 2 (digitToInt y)
        'c' -> Just $ Coord 3 (digitToInt y)
        'd' -> Just $ Coord 4 (digitToInt y)
        'e' -> Just $ Coord 5 (digitToInt y)
        'f' -> Just $ Coord 6 (digitToInt y)
        'g' -> Just $ Coord 7 (digitToInt y)
        'h' -> Just $ Coord 8 (digitToInt y)
        _ -> Nothing
parseCoord _ = Nothing


printMoveValues :: [Move] -> Game -> String
printMoveValues mvs g = unwords . map helper . map (move g) $ mvs
        where helper (Just gm) = show $ evalNaive gm
              helper Nothing = []
              
alphaBetaT :: Game -> Move
alphaBetaT g = maxGame $ map (\mv -> (abNode 6 (-100) 100 (move g mv) False, mv)) (nextMoves g)
        where maxGame = snd . foldr foldingF (-100, Move (Coord 1 1) (Coord 1 1))
              foldingF x y = if fst y > fst x then y else x