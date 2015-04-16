module Enpassant where

import ChessBoard
import ChessRules
import Control.Applicative

--can assume that the move is valid (pawn is attacking the space) and prev is the opponent's previous move
enPassant :: (Game, Move) -> Move -> Maybe (Game, Move)
enPassant arg@(Game b _ _, mv@(Move _ c2)) prev
        | wasPawnTwoSpMove b prev && getP b c2 == Nothing && validEPSpace prev mv = Just arg
        | otherwise = Nothing

wasPawnTwoSpMove :: Board -> Move -> Bool
wasPawnTwoSpMove b (Move (Coord _ y1) c2@(Coord _ y2)) = maybeBool (pawn <$> getP b c2) && abs (y2 - y1) == 2  
          
--first argument is opponent's prev, second is current
validEPSpace :: Move -> Move -> Bool
validEPSpace prev (Move _ c2) = vertSpBetween prev == c2
        where vertSpBetween (Move (Coord x y) c2') = if Coord x y `northOf` c2'
                then Coord (x + 1) y else Coord (x - 1) y
                
maybeBool :: Maybe Bool -> Bool
maybeBool (Just x) = x
maybeBool Nothing = False