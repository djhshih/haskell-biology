{-# LANGUAGE OverloadedStrings #-}

module Bio.Seq.AA
    where


import qualified Bio.Seq.String as S


data Seq = Seq !S.String
    deriving (Show)


alphabet :: [Char]
alphabet = "ARNDCEQGHILKMFPSTWYV"

-- Drop the stop codon and subsequent residues.
dropStop :: Seq -> Seq
dropStop (Seq xs) = Seq $ S.takeWhile (/= '*') xs

-- Check whether amino acid sequence is valid.
valid :: Seq -> Bool
valid (Seq xs) = S.null r || r == "*"
  where
    r = S.dropWhile (`elem` alphabet) xs

