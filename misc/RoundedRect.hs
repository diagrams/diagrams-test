{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

main = defaultMain (roundedRect' 5 3 with { radiusTL = 2, radiusBL = 1 } # pad 1.1)