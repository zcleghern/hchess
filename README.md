# hchess
Haskell chess program.  In a very rough state.

build.bat just runs ghc if you don't want to use cabal (had it here before I was using cabal)

Currently the other player is using minimax with alpha-beta pruning (not really working).

TODO:

1) Improve board evaluation
2) Fix alpha-beta pruning algorithm
3) Option for 2 players; vs. AI with random starting color
4) Refactoring modules
5) GUI using functional reactive programming with Sodium
