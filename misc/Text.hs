{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine

helloWorld :: Diagram B
helloWorld = text "Hello world!" <> rect 8 1

aligned :: Diagram B
aligned = t1 =/= t2 =/= t3
  where
    pt = circle 0.1 # fc red
    t1 = pt <> topLeftText         "top left"   <> rect 8 1
    t2 = pt <> baselineText        "baseline"   <> rect 8 1
    t3 = pt <> alignedText 0.7 0.5 "(0.7, 0.5)" <> rect 8 1
    d1 =/= d2 = d1 === strutY 2 === d2

attributes :: Diagram B
attributes =
       center $
       text' 10 "Hello" # italic
   === text' 5 "there"  # bold # font "freeserif"
   === text' 3 "world"  # fc green
   where
     text' s t = text t # fontSizeL s <> strutY (s * 1.3)

sizeLocal :: Diagram B
sizeLocal =
  hcat [eff, eff # scale 2, eff # scaleX 2, eff # scaleY 2, eff # rotateBy (1/12)]
                 # fontSizeL 1
  where
    eff = text "F" <> square 1

sizeNormalized :: Diagram B
sizeNormalized =
  hcat [eff, eff # scale 2, eff # scaleX 2, eff # scaleY 2, eff # rotateBy (1/12)]
                 # fontSizeN 0.1
  where
    eff = text "F" <> square 1

main = mainWith $ ( helloWorld === strutY 1
                === aligned    === strutY 1
                -- === attributes
                === sizeLocal  === strutY 2
                === sizeNormalized ) # frame 1
