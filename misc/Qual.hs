{-# LANGUAGE DeriveDataTypeable #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.Typeable

data Corner = NW | NE | SW | SE
  deriving (Typeable, Eq, Ord, Show)
instance IsName Corner

connect n1 n2
  = withName n1 $ \b1 ->
    withName n2 $ \b2 ->
      atop ((location b1 ~~ location b2) # lc red # lw 0.03)

squares =  (s # named NW ||| s # named NE)
       === (s # named SW ||| s # named SE)
  where s = square 1

d = hcat' with {sep = 0.5} (zipWith (|>) [0::Int ..] (replicate 5 squares))

pairs :: [(Name, Name)]
pairs = [ ((0::Int) .> NE, (2::Int) .> SW)
        , ((1::Int) .> SE, (4::Int) .> NE)
        , ((3::Int) .> NW, (3::Int) .> SE)
        , ((0::Int) .> SE, (1::Int) .> NW)
        ]

example = d # applyAll (map (uncurry connect) pairs)

main = defaultMain example