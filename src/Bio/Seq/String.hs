{-# LANGUAGE OverloadedStrings #-}

module Bio.Seq.String
    ( module Bio.Seq.String
    , module Data.ByteString.Lazy.Char8
    ) where


import Data.ByteString.Lazy.Char8
import Data.ByteString.Lazy.Char8 as S

type String = S.ByteString

chunksOf :: Int -> S.ByteString -> [S.ByteString]
chunksOf n xs =
    if S.null xs then []
    else (first) : (chunksOf n $ rest)
        where (first, rest) = S.splitAt (fromIntegral n) xs

