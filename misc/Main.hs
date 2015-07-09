module Main where

import           Test.Tasty

import qualified CSG
import qualified Quickcheck

main = defaultMain $ testGroup "Diagrams"
              [ Quickcheck.tests
              , CSG.tests
              ]
