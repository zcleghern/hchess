
module ChessBoard where

import Data.Array

data Color = White | Black deriving (Eq)
data Coord = Coord Int Int
data Piece = Pawn Color | Rook Color | Knight Color | Bishop Color | Queen Color | King Color | Empty deriving (Eq)
type Board = Array Int (Array Int Piece)

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
        show Empty = "  "
        
printBoard :: Board -> String
printBoard b = foldl f "" (elems b)
        where f l r = l ++ "\n" ++ printRank r
        
printRank :: Array Int Piece -> String
printRank a = foldl f "" (elems a)
        where f l r = l ++ " " ++ show r

blankBoard :: Board
blankBoard = array (1,8) [(i, array (1,8) [(j,Empty) | j <- [1..8]]) | i <- [1..8]]

initBoard :: Board
initBoard = chessArray [row1, row2, rowMid, rowMid, rowMid, rowMid, row7, row8]
        where row1 = chessArray $ white backRow
              row2 = chessArray $ white pawnRow
              rowMid = chessArray emptyRow
              row7 = chessArray $ black pawnRow
              row8 = chessArray $ black backRow
              chessArray = array (1,8) . zip [1..8]
              white = map ($ White)
              black = map ($ Black)
              emptyRow = replicate 8 Empty
              pawnRow = replicate 8 Pawn
              backRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

removeP :: Board -> Int -> Int -> Board
removeP = addP Empty
        
addP :: Piece -> Board -> Int -> Int -> Board
addP p b row col = b//[(row, oldRow//[(col, p)])]
        where oldRow = (!) b row

row :: Board -> Int -> [Piece]
row b r = elems $ (!) b r

col :: Board -> Int -> [Piece]
col b c = map (get c) $ elems b
        where get j a = (!) a j
        
getP :: Board -> Coord -> Piece
getP b (Coord x y) = (!) ((!) b y) x
        
color :: Piece -> Color
color (Pawn c) = c

adj :: Coord -> Coord -> Bool
adj (Coord x1 y1) (Coord x2 y2)
        | y1 == y2 = x1 == x2 + 1 || x1 == x2 - 1
        | x1 == x2 = y1 == y2 + 1 || y1 == y2 - 1
        | otherwise = False

diag :: Coord -> Coord -> Bool
diag (Coord x1 y1) (Coord x2 y2) = (x1 == x2 + 1 || x1 == x2 - 1) && (y1 == y2 + 1 || y1 == y2 - 1)