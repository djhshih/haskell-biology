{-# LANGUAGE OverloadedStrings #-}

module Bio.Seq.AA
    where


import qualified Bio.Seq.String as S


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
alphabet = "ARNDCEQGHILKMFPSTWYV"

-- Drop the stop codon and subsequent residues.
dropStop :: Seq -> Seq
dropStop (Seq xs) = Seq $ S.takeWhile (/= '*') xs

-- Check whether amino acid sequence is valid.
valid :: Seq -> Bool
valid (Seq xs) = S.null r || r == "*"
  where
    r = S.dropWhile (`elem` alphabet) xs

