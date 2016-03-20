module Main where

import           Test.Tasty

import qualified CSG

main = defaultMain $ testGroup "Diagrams"
              [ CSG.tests
              ]
