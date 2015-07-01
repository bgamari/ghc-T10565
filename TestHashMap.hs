{-# LANGUAGE OverloadedStrings #-}

module TestHashMap where

import qualified Data.ByteString.Char8 as BS
import Data.HashMap.Strict as M
import Data.Maybe

{-# NOINLINE guessElement #-}
-- | Guessing an 'Element' name from PDB 'Atom' name. 
-- Returns empty string, if 'Element' can't be guessed.
guessElement :: BS.ByteString -> String
guessElement = \e -> fromMaybe "" $ M.lookup e els
  where
    els = M.fromList
        [ ("C"   , "C"),
          ("C1'" , "C"),
          ("C2"  , "C"),
          ("C2'" , "C"),
          ("C3'" , "C"),
          ("C4"  , "C"),
          ("C4'" , "C"),
          ("C5"  , "C"),
          ("C5'" , "C"),
          ("C6"  , "C"),
          ("C8"  , "C"),
          ("CA"  , "C"),
          ("CB"  , "C"),
          ("CD"  , "C"),
          ("CD1" , "C"),
          ("CD2" , "C"),
          ("CE"  , "C"),
          ("CE1" , "C"),
          ("CE2" , "C"),
          ("CE3" , "C"),
          ("CG"  , "C"),
          ("CG1" , "C"),
          ("CG2" , "C"),
          ("CH2" , "C"),
          ("CZ"  , "C"),
          ("CZ2" , "C"),
          ("CZ3" , "C"),
          ("N"   , "N"),
          ("N1"  , "N"),
          ("N2"  , "N"),
          ("N3"  , "N"),
          ("N4"  , "N"),
          ("N6"  , "N"),
          ("N7"  , "N"),
          ("N9"  , "N"),
          ("ND1" , "N"),
          ("ND2" , "N"),
          ("NE"  , "N"),
          ("NE1" , "N"),
          ("NE2" , "N"),
          ("NH1" , "N"),
          ("NH2" , "N"),
          ("NZ"  , "N"),
          ("O"   , "O"),
          ("O2"  , "O"),
          ("O2'" , "O"),
          ("O3'" , "O"),
          ("O4"  , "O"),
          ("O4'" , "O"),
          ("O5'" , "O"),
          ("O6"  , "O"),
          ("OD1" , "O"),
          ("OD2" , "O"),
          ("OE1" , "O"),
          ("OE2" , "O"),
          ("OG"  , "O"),
          ("OG1" , "O"),
          ("OH"  , "O"),
          ("OP1" , "O"),
          ("OP2" , "O"),
          ("OXT" , "O"),
          ("P"   , "P"),
          ("SD"  , "S"),
          ("SG"  , "S")]
