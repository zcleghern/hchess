module ChessBoard where

data Color = White | Black deriving (Show, Eq)
data Piece  = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show, Eq)

initBoard :: [[Maybe (Color, Piece)]]
initBoard = [[Just (White, Rook), Just (White, Knight), Just (White, Bishop), Just (White, Queen), Just (White, King), Just (White, Bishop), Just (White, Knight), Just (White, Rook)],
             [Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn)],
             replicate 8 Nothing,
             replicate 8 Nothing,
             replicate 8 Nothing,
             replicate 8 Nothing,
             [Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn)],
             [Just (Black, Rook), Just (Black, Knight), Just (Black, Bishop), Just (Black, Queen), Just (Black, King), Just (Black, Bishop), Just (Black, Knight), Just (Black, Rook)]]
 
attacking :: [[Maybe (Color, Piece)]] -> Int -> Int -> Int -> Int -> Bool
attacking b r c r1 c1
        | b !! r !! c == Nothing = False
        | b !! r !! c == Just (White, Pawn) = pawnAttacking White r c r1 c1
        | b !! r !! c == Just (Black, Pawn) = pawnAttacking Black r c r1 c1
        
pawnAttacking :: Color -> Int -> Int -> Int -> Int -> Bool
pawnAttacking color r c r1 c1
        | color == White = (c == c1 + 1 || c == c1 - 1) && r == r1 - 1
        | otherwise = (c == c1 + 1 || c == c1 - 1) && r == r1 + 1
        
rookAttacking :: [[Maybe (Color, Piece)]] -> Int -> Int -> Int -> Int -> Bool
rookAttacking r c r1 c1 = 