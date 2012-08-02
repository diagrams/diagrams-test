{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import System.Environment

main = withArgs [ "-o", "test4.png", "-w", "400", "-h", "400" ] $ defaultMain $ ((text "ABCDEFGHabcdefgh" # fontSize 2 # translateX 8 <> rect 10 1 # lw 0.1) # translateX (-5)) <> rect 12 12 # lw 0.2