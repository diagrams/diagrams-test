{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

main = defaultMain $ circle 1 ||| (mempty <> (translateX 4 $ circle 2))