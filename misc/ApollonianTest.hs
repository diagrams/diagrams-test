import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Apollonian

main = defaultMain (apollonianGasket (1/500) 1 1 1)