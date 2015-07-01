{-# LANGUAGE OverloadedStrings #-}
module TestOrig where

import qualified Data.ByteString.Char8 as BS

{-# NOINLINE guessElement #-}
-- | Guessing an 'Element' name from PDB 'Atom' name. 
-- Returns empty string, if 'Element' can't be guessed.
guessElement :: BS.ByteString -> BS.ByteString
guessElement "C"   = "C"
guessElement "C1'" = "C"
guessElement "C2"  = "C"
guessElement "C2'" = "C"
guessElement "C3'" = "C"
guessElement "C4"  = "C"
guessElement "C4'" = "C"
guessElement "C5"  = "C"
guessElement "C5'" = "C"
guessElement "C6"  = "C"
guessElement "C8"  = "C"
guessElement "CA"  = "C"
guessElement "CB"  = "C"
guessElement "CD"  = "C"
guessElement "CD1" = "C"
guessElement "CD2" = "C"
guessElement "CE"  = "C"
guessElement "CE1" = "C"
guessElement "CE2" = "C"
guessElement "CE3" = "C"
guessElement "CG"  = "C"
guessElement "CG1" = "C"
guessElement "CG2" = "C"
guessElement "CH2" = "C"
guessElement "CZ"  = "C"
guessElement "CZ2" = "C"
guessElement "CZ3" = "C"
guessElement "N"   = "N"
guessElement "N1"  = "N"
guessElement "N2"  = "N"
guessElement "N3"  = "N"
guessElement "N4"  = "N"
guessElement "N6"  = "N"
guessElement "N7"  = "N"
guessElement "N9"  = "N"
guessElement "ND1" = "N"
guessElement "ND2" = "N"
guessElement "NE"  = "N"
guessElement "NE1" = "N"
guessElement "NE2" = "N"
guessElement "NH1" = "N"
guessElement "NH2" = "N"
guessElement "NZ"  = "N"
guessElement "O"   = "O"
guessElement "O2"  = "O"
guessElement "O2'" = "O"
guessElement "O3'" = "O"
guessElement "O4"  = "O"
guessElement "O4'" = "O"
guessElement "O5'" = "O"
guessElement "O6"  = "O"
guessElement "OD1" = "O"
guessElement "OD2" = "O"
guessElement "OE1" = "O"
guessElement "OE2" = "O"
guessElement "OG"  = "O"
guessElement "OG1" = "O"
guessElement "OH"  = "O"
guessElement "OP1" = "O"
guessElement "OP2" = "O"
guessElement "OXT" = "O"
guessElement "P"   = "P"
guessElement "SD"  = "S"
guessElement "SG"  = "S"
guessElement _     = "" -- not a standard residue of protein or nucleic acid1G
