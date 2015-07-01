import Criterion.Main

import qualified Data.ByteString.Char8 as BS

import qualified TestAssoc
import qualified TestStringAssoc
import qualified TestMap
import qualified TestStringMap
import qualified TestHashMap
import qualified TestStringHashMap
import qualified TestOrig
import qualified TestString

main = defaultMain
       [ bench "Assoc"          $ nf (map TestAssoc.guessElement) tests
       , bench "StringAssoc"    $ nf (map TestStringAssoc.guessElement) tests
       , bench "Orig"           $ nf (map TestOrig.guessElement) tests
       , bench "Map"            $ nf (map TestMap.guessElement) tests
       , bench "String"         $ nf (map TestString.guessElement) tests
       , bench "StringMap"      $ nf (map TestStringMap.guessElement) tests
       , bench "StringHashMap"  $ nf (map TestHashMap.guessElement) tests
       , bench "HashMap"        $ nf (map TestStringHashMap.guessElement) tests
       ]

tests = map BS.pack
       [ "C"
       , "C1'"
       , "C2"
       , "C2'"
       , "C3'"
       , "C4"
       , "C4'"
       , "C5"
       , "C5'"
       , "C6"
       , "C8"
       , "CA"
       , "CB"
       , "CD"
       , "CD1"
       , "CD2"
       , "CE"
       , "CE1"
       , "CE2"
       , "CE3"
       , "CG"
       , "CG1"
       , "CG2"
       , "CH2"
       , "CZ"
       , "CZ2"
       , "CZ3"
       , "N"
       , "N1"
       , "N2"
       , "N3"
       , "N4"
       , "N6"
       , "N7"
       , "N9"
       , "ND1"
       , "ND2"
       , "NE"
       , "NE1"
       , "NE2"
       , "NH1"
       , "NH2"
       , "NZ"
       , "O"
       , "O2"
       , "O2'"
       , "O3'"
       , "O4"
       , "O4'"
       , "O5'"
       , "O6"
       , "OD1"
       , "OD2"
       , "OE1"
       , "OE2"
       , "OG"
       , "OG1"
       , "OH"
       , "OP1"
       , "OP2"
       , "OXT"
       , "P"
       , "SD"
       , "SG"
       ]
