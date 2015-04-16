
module ChessBoard where

import Data.Array
import Control.Applicative

data Color = White | Black deriving (Eq)
data Coord = Coord Int Int deriving (Eq)
data Piece = Pawn Color | Rook Color | Knight Color | Bishop Color | Queen Color | King Color deriving (Eq)
type Board = Array Int (Array Int (Maybe Piece))
data Move = Move Coord Coord deriving (Eq)

instance Show Coord where
        show (Coord x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Show Move where
        show (Move src dest) = show src ++ " => " ++ show dest

instance Show Color where
        show White = "w"
        show Black = "b"

instance Show Piece where
        show (King c) = show c ++ "K"
        show (Queen c) = show c ++ "Q"
        show (Bishop c) = show c ++ "B"
        show (Knight c) = show c ++ "N"
        show (Rook c) = show c ++ "R"
        show (Pawn c) = show c ++ "P"
        
notC :: Color -> Color
notC White = Black
notC Black = White
        
allSpaces :: [Coord]
allSpaces = [Coord x y | x <- [1..8], y <- [1..8]]

printBoard :: Board -> String
printBoard b = fst $ foldl f ("  a  b  c  d  e  f  g  h", 1::Int) (elems b)
        where f (l,i) r = (l ++ "\n" ++ show i ++ printRank r, i+1)
        
show' :: Maybe Piece -> String
show' (Just p) = show p
show' Nothing = "  "

printRank :: Array Int (Maybe Piece) -> String
printRank a = foldl f "" (elems a)
        where f l r = l ++ " " ++ show' r

blankBoard :: Board
blankBoard = array (1,8) [(i, array (1,8) [(j,Nothing) | j <- [1..8]]) | i <- [1..8]]

initBoard :: Board
initBoard = chessArray [row1, row2, rowMid, rowMid, rowMid, rowMid, row7, row8]
        where row1 = chessArray . map Just $ white backRow
              row2 = chessArray . map Just $ white pawnRow
              rowMid = chessArray emptyRow
              row7 = chessArray . map Just $ black pawnRow
              row8 = chessArray . map Just $ black backRow
              chessArray = array (1,8) . zip [1..8]
              white = map ($ White)
              black = map ($ Black)
              emptyRow = replicate 8 Nothing
              pawnRow = replicate 8 Pawn
              backRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

moveP :: Board -> Move -> Maybe Board
moveP b (Move c1 c2)
        | outOfBounds c1 || outOfBounds c2 = Nothing
        | otherwise = let Just p = getP b c1 in Just (addP p (removeP b c1) c2)
        
addP :: Piece -> Board -> Coord -> Board
addP p b (Coord c r) = b//[(r, oldRow//[(c, Just p)])]
        where oldRow = (!) b r
        
removeP :: Board -> Coord -> Board
removeP b (Coord c r) = b//[(r, oldRow//[(c, Nothing)])]
        where oldRow = (!) b r
        
westOf :: Coord -> Coord -> Bool
westOf (Coord x1 _) (Coord x2 _) = x1 < x2

eastOf :: Coord -> Coord -> Bool
eastOf (Coord x1 _) (Coord x2 _) = x1 > x2

northOf :: Coord -> Coord -> Bool
northOf (Coord _ y1) (Coord _ y2) = y1 < y2

southOf :: Coord -> Coord -> Bool
southOf (Coord _ y1) (Coord _ y2) = y1 > y2

row :: Board -> Int -> [Maybe Piece]
row b r = elems $ (!) b r

col :: Board -> Int -> [Maybe Piece]
col b c = map (get c) $ elems b
        where get j a = (!) a j
        
getP :: Board -> Coord -> Maybe Piece
getP b c@(Coord x y) 
        | outOfBounds c = Nothing
        | otherwise = (!) ((!) b y) x
        
color :: Piece -> Color
color (Pawn c) = c
color (Rook c) = c
color (Knight c) = c
color (Bishop c) = c
color (Queen c) = c
color (King c) = c

pieceType :: Piece -> String
pieceType (Pawn _) = "Pawn"
pieceType (Rook _) = "Rook"
pieceType (Knight _) = "Knight"
pieceType (Bishop _) = "Bishop"
pieceType (Queen _) = "Queen"
pieceType (King _) = "King"

isOpposite :: Board -> Color -> Coord -> Bool
isOpposite b colr coord = let otherCol = color <$> getP b coord in
        otherCol /= Just colr && otherCol /= Nothing


outOfBounds :: Coord -> Bool
outOfBounds (Coord x y) = x < 1 || x > 8 || y < 1 || y > 8

pawn :: Piece -> Bool
pawn (Pawn _) = True
pawn _ = False

king :: Piece -> Bool
king (King _) = True
king _ = False

getKing :: Board -> Color -> Coord
getKing b c = case findPieces b (King c) of
        x:_ -> x
        [] -> Coord 0 0

findPieces :: Board -> Piece -> [Coord]
findPieces b p = loop [] [1..8] (\lst x -> 
                        loop lst [1..8] (\lst2 y -> 
                                if Just p == getP b (Coord x y) then Coord x y : lst2 else lst2))
                                
loop :: a -> [b] -> (a -> b -> a) -> a
loop initial list f = foldl f initial list

adj :: Coord -> Coord -> Bool
adj (Coord x1 y1) (Coord x2 y2)
        | y1 == y2 = x1 == x2 + 1 || x1 == x2 - 1
        | x1 == x2 = y1 == y2 + 1 || y1 == y2 - 1
        | otherwise = False

diag :: Coord -> Coord -> Bool
diag (Coord x1 y1) (Coord x2 y2) = (x1 == x2 + 1 || x1 == x2 - 1) && (y1 == y2 + 1 || y1 == y2 - 1)