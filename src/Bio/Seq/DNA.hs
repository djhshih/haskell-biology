{-# LANGUAGE OverloadedStrings #-}

module Bio.Seq.DNA
  where


import Prelude hiding (concat)
import qualified Data.Map as M

import qualified Bio.Seq.String as S
import qualified Bio.Seq.RNA as RNA
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
alphabet = "ACGT"

complement :: Char -> Char
complement x = case x of
    'A' -> 'T'
    'C' -> 'G'
    'G' -> 'C'
    'T' -> 'A'
    _ -> 'N'

revComplement :: Seq -> Seq
revComplement (Seq xs) = Seq $ S.map complement $ S.reverse xs

transcribe :: Seq -> [(Int, Int)] -> RNA.Seq
transcribe seq exons = RNA.splice (toRNA seq) exons

toRNA :: Seq -> RNA.Seq
toRNA (Seq xs) = RNA.Seq $ S.map (\x -> if x == 'T' then 'U' else x) xs

fromRNA :: RNA.Seq -> Seq
fromRNA (RNA.Seq xs) = Seq $ S.map (\x -> if x == 'U' then 'T' else x) xs

codonToAA :: S.String -> Char
codonToAA c = case c of
    "TTT" -> 'F'
    "TTC" -> 'F'
    "TTA" -> 'L'
    "TTG" -> 'L'
    "CTT" -> 'L'
    "CTC" -> 'L'
    "CTA" -> 'L'
    "CTG" -> 'L'
    "ATT" -> 'I'
    "ATC" -> 'I'
    "ATA" -> 'I'
    "ATG" -> 'M'
    "GTT" -> 'V'
    "GTC" -> 'V'
    "GTA" -> 'V'
    "GTG" -> 'V'
    "TCT" -> 'S'
    "TCC" -> 'S'
    "TCA" -> 'S'
    "TCG" -> 'S'
    "CCU" -> 'P'
    "CCC" -> 'P'
    "CCA" -> 'P'
    "CCG" -> 'P'
    "ACT" -> 'T'
    "ACC" -> 'T'
    "ACA" -> 'T'
    "ACG" -> 'T'
    "GCT" -> 'A'
    "GCC" -> 'A'
    "GCA" -> 'A'
    "GCG" -> 'A'
    "TAT" -> 'Y'
    "TAC" -> 'Y'
    "TAA" -> '*'
    "TAG" -> '*'
    "CAT" -> 'H'
    "CAC" -> 'H'
    "CAA" -> 'Q'
    "CAG" -> 'Q'
    "AAT" -> 'N'
    "AAC" -> 'N'
    "AAA" -> 'K'
    "AAG" -> 'K'
    "GAT" -> 'D'
    "GAC" -> 'D'
    "GAA" -> 'E'
    "GAG" -> 'E'
    "TGT" -> 'C'
    "TGC" -> 'C'
    "TGA" -> '*'
    "TGG" -> 'W'
    "CGT" -> 'R'
    "CGC" -> 'R'
    "CGA" -> 'R'
    "CGG" -> 'R'
    "AGT" -> 'S'
    "AGC" -> 'S'
    "AGA" -> 'R'
    "AGG" -> 'R'
    "GGT" -> 'G'
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

