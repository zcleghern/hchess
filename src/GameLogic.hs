module GameLogic where

import ChessBoard
import ChessRules
import Castling
import Enpassant
import Control.Applicative
import Data.Maybe

data GameStatus = WhiteMated | BlackMated | Stalemate | WhiteTurn | BlackTurn

move :: Game -> Move -> Maybe Game
move g@(Game b fl t) mv = if isCorrectColor g mv then makeMove (Game b fl (notC t)) mv else Nothing

status :: Game -> GameStatus
status g@(Game b fl t)
        | checkMate g White = WhiteMated
        | checkMate g Black = BlackMated
        | t == White = WhiteTurn
        | t == Black = BlackTurn

checkMate :: Game -> Color -> Bool
checkMate g@(Game b fl t) clr
        | t /= clr = False
        | otherwise = inCheck b t && noMovesAvailable g
                where noMovesAvailable g = null . filter isJust . map (move g) $ concatMap (possibleMoves g) allSpaces

isCorrectColor :: Game -> Move -> Bool
isCorrectColor (Game b _ t) (Move c1 _) = Just t == (color <$> getP b c1)

makeMove :: Game -> Move -> Maybe Game
makeMove g@(Game b fl t) mv@(Move c1 c2)
        | maybeBool (king <$> getP b c1) && not (attacking b c1 c2) = castle g mv
        | maybeBool (pawn <$> getP b c1) && not (diag c1 c2) && getP b c2 == Nothing = pawnStrMove g mv --pawn moving straight ahead
        | maybeBool (pawn <$> getP b c1) && getP b c2 == Nothing = Nothing -- pawn not moving straight ahead, is attack
        | attacking b c1 c2 && (color <$> getP b c1) /= (color <$> getP b c2) = moveP b mv >>= \mb -> return (Game mb fl t)        
        | otherwise = Nothing
        
       
pawnStrMove :: Game -> Move -> Maybe Game
pawnStrMove (Game b fl t) mv@(Move c1@(Coord x1 y1) (Coord x2 y2)) = case color <$> getP b c1 of
        Just White -> if x1 == x2 && (y1 == y2 - 1 || y1 == 2 && y2 == 4) then moveP b mv >>= \mb -> return (Game mb fl t) else Nothing
        Just Black -> if x1 == x2 && (y1 == y2 + 1 || y1 == 7 && y2 == 5) then moveP b mv >>= \mb -> return (Game mb fl t) else Nothing
        Nothing -> Nothing
        
updateFlags :: (Game, Move) -> (Game, Move)
updateFlags (Game b fl t, mv@(Move c1 _))
        | c1 == Coord 1 1 && getP b c1 == Just (Rook White) = (Game b ("a1Rook":fl) t, mv)
        | c1 == Coord 8 1 && getP b c1 == Just (Rook White) = (Game b ("h1Rook":fl) t, mv)
        | c1 == Coord 1 8 && getP b c1 == Just (Rook Black) = (Game b ("a8Rook":fl) t, mv)
        | c1 == Coord 8 8 && getP b c1 == Just (Rook Black) = (Game b ("h8Rook":fl) t, mv)
        | getP b c1 == Just (King White) = (Game b ("wKing":fl) t, mv)
        | getP b c1 == Just (King Black) = (Game b ("bKing":fl) t, mv)
        | otherwise = (Game b fl t, mv)
        
possibleMoves :: Game -> Coord -> [Move]
possibleMoves (Game b _ clr) c
        | p == Just (Pawn clr)    = pawnMoves c b
        | p == Just (Knight clr)  = knightMoves c
        | p == Just (Bishop clr)  = bishopMoves c
        | p == Just (Rook clr)    = rookMoves c
        | p == Just (Queen clr)   = queenMoves c
        | p == Just (King clr)    = kingMoves c
        | otherwise = []
                where p = getP b c
        
pawnMoves :: Coord -> Board -> [Move]
pawnMoves c@(Coord x y) b = case color <$> getP b c of
        Just Black -> map (Move c) $ filter inBounds [Coord x (y - 1), Coord x (y - 2), Coord (x + 1) (y - 1), Coord (x - 1) (y - 1)]
        Just White -> map (Move c) $ filter inBounds [Coord x (y + 1), Coord x (y + 2), Coord (x + 1) (y + 1), Coord (x - 1) (y + 1)]
        Nothing -> []

knightMoves :: Coord -> [Move]
knightMoves c@(Coord x y) =  map (Move c) $ filter inBounds [
        Coord (x + 2) (y + 1), Coord (x + 1) (y + 2),
        Coord (x - 1) (y + 2), Coord (x - 2) (y + 1),
        Coord (x - 2) (y - 1), Coord (x - 1) (y - 2),
        Coord (x + 1) (y - 2), Coord (x + 2) (y - 1)]
        
bishopMoves :: Coord -> [Move]
bishopMoves c = map (Move c) $ movesNW (nw c) ++ movesNE (ne c) ++ movesSW (sw c) ++ movesSE (se c)
        where movesNW c = if outOfBounds c then [] else c:movesNW (nw c)
              nw (Coord x y) = Coord (x - 1) (y - 1)
              movesNE c = if outOfBounds c then [] else c:movesNE (ne c)
              ne (Coord x y) = Coord (x + 1) (y - 1)
              movesSW c = if outOfBounds c then [] else c:movesSW (sw c)
              sw (Coord x y) = Coord (x - 1) (y + 1)
              movesSE c = if outOfBounds c then [] else c:movesSE (se c)
              se (Coord x y) = Coord (x + 1) (y + 1)
              
rookMoves :: Coord -> [Move]
rookMoves c = map (Move c) $ movesW (w c) ++ movesS (s c) ++ movesE (e c) ++ movesN (n c)
        where movesW c = if outOfBounds c then [] else c:movesW (w c)
              w (Coord x y) = Coord (x - 1) y
              movesS c = if outOfBounds c then [] else c:movesS (s c)
              s (Coord x y) = Coord x (y + 1)
              movesE c = if outOfBounds c then [] else c:movesE (e c)
              e (Coord x y) = Coord (x + 1) y
              movesN c = if outOfBounds c then [] else c:movesN (n c)
              n (Coord x y) = Coord x (y - 1)
              
queenMoves :: Coord -> [Move]
queenMoves c = rookMoves c ++ bishopMoves c

kingMoves :: Coord -> [Move]
kingMoves c@(Coord x y) = map (Move c) [Coord (x - 1) (y - 1), Coord (x - 1) y, Coord (x - 1) (y + 1),
                         Coord x (y + 1), Coord (x + 1) (y + 1), Coord (x + 1) y,
                         Coord (x + 1) (y + 1), Coord x (y + 1)]
                         
inBounds :: Coord -> Bool
inBounds = not . outOfBounds