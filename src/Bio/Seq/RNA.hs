{-# LANGUAGE OverloadedStrings #-}

module Bio.Seq.RNA
    where

import Prelude hiding (concat)
import qualified Data.Map as M

import qualified Bio.Seq.String as S
import qualified Bio.Seq.AA as AA


data Seq = Seq !S.String
    deriving (Show)

empty :: Seq
empty = Seq (S.empty)

length :: Seq -> Int
length (Seq xs) = fromIntegral $ S.length xs

join :: Seq -> Seq -> Seq
join (Seq xs) (Seq ys) =  Seq $ S.concat [xs, ys]

concat :: [Seq] -> Seq
concat = foldr join empty

-- range is [start, end) and 0-based
slice :: Seq -> (Int, Int) -> Seq
slice (Seq xs) (start, end) = Seq $ S.take (e - s) $ snd $ S.splitAt s xs
  where
    s = fromIntegral start
    e = fromIntegral end

--

alphabet :: [Char]
alphabet = "ACGU"

valid :: Seq -> Bool
valid (Seq xs) = S.null $ S.dropWhile (`elem` alphabet) xs


splice :: Seq -> [(Int, Int)] -> Seq
splice seq exons = concat $ map (\r -> slice seq r) exons

complement :: Char -> Char
complement x = case x of
    'A' -> 'U'
    'C' -> 'G'
    'G' -> 'C'
    'U' -> 'A'
    _ -> 'N'

revComplement :: Seq -> Seq
revComplement (Seq xs) = Seq (S.map complement $ S.reverse xs)

codonToAA :: S.String -> Char
codonToAA c = case c of
    "UUU" -> 'F'
    "UUC" -> 'F'
    "UUA" -> 'L'
    "UUG" -> 'L'
    "CUU" -> 'L'
    "CUC" -> 'L'
    "CUA" -> 'L'
    "CUG" -> 'L'
    "AUU" -> 'I'
    "AUC" -> 'I'
    "AUA" -> 'I'
    "AUG" -> 'M'
    "GUU" -> 'V'
    "GUC" -> 'V'
    "GUA" -> 'V'
    "GUG" -> 'V'
    "UCU" -> 'S'
    "UCC" -> 'S'
    "UCA" -> 'S'
    "UCG" -> 'S'
    "CCU" -> 'P'
    "CCC" -> 'P'
    "CCA" -> 'P'
    "CCG" -> 'P'
    "ACU" -> 'T'
    "ACC" -> 'T'
    "ACA" -> 'T'
    "ACG" -> 'T'
    "GCU" -> 'A'
    "GCC" -> 'A'
    "GCA" -> 'A'
    "GCG" -> 'A'
    "UAU" -> 'Y'
    "UAC" -> 'Y'
    "UAA" -> '*'
    "UAG" -> '*'
    "CAU" -> 'H'
    "CAC" -> 'H'
    "CAA" -> 'Q'
    "CAG" -> 'Q'
    "AAU" -> 'N'
    "AAC" -> 'N'
    "AAA" -> 'K'
    "AAG" -> 'K'
    "GAU" -> 'D'
    "GAC" -> 'D'
    "GAA" -> 'E'
    "GAG" -> 'E'
    "UGU" -> 'C'
    "UGC" -> 'C'
    "UGA" -> '*'
    "UGG" -> 'W'
    "CGU" -> 'R'
    "CGC" -> 'R'
    "CGA" -> 'R'
    "CGG" -> 'R'
    "AGU" -> 'S'
    "AGC" -> 'S'
    "AGA" -> 'R'
    "AGG" -> 'R'
    "GGU" -> 'G'
    "GGC" -> 'G'
    "GGA" -> 'G'
    "GGG" -> 'G'
    x | S.length x == 3 -> 'X'
      | otherwise       -> '?'

toAA :: Seq -> AA.Seq
toAA (Seq xs) = AA.Seq $ S.pack $ map codonToAA (S.chunksOf 3 xs)

-- Translate with a non-standard genetic code.
toAAWith :: M.Map S.String Char -> Seq -> AA.Seq
toAAWith m (Seq xs) = AA.Seq $ S.pack $
    map (\codon -> maybe '?' id (M.lookup codon m)) (S.chunksOf 3 xs)

-- Translate RNA transcript into peptide.
-- Produces empty sequence if transcript is not valid
translate :: Seq -> [(Int, Int)] -> AA.Seq
translate seq exons =
    if (not . S.null) translated &&
       S.last translated == '*' &&
       AA.valid (AA.Seq translated)
    then AA.dropStop $ AA.Seq translated
    else AA.empty
  where
    -- Replace first codon with canonical start codon,
    -- because some genes use a non-standard start codon (first AA is still M)
    translated = S.pack $ map codonToAA ("AUG" : (S.chunksOf 3 dropped))
    dropped = S.drop 3 spliced
    Seq spliced = splice seq exons

