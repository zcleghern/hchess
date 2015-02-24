module Game where

import ChessBoard
import qualified Data.Map as M

data Game = Game Board Flags
type Flags = M.Map String Bool

safeLookup :: String -> Flags -> Bool
safeLookup key = case M.lookup key of
        Just x -> x
        Nothing -> False

a1RookCastleAvailable :: Flags -> Bool
a1RookCastleAvailable = safeLookup "a1Rook"

h1RookCastleAvailable :: Flags -> Bool
h1RookCastleAvailable = safeLookup "h1Rook"

a8RookCastleAvailable :: Flags -> Bool
a8RookCastleAvailable = safeLookup "a8Rook"

h8RookCastleAvailable :: Flags -> Bool
h8RookCastleAvailable = safeLookup "h8Rook"

