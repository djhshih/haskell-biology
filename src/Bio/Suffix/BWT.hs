module Bio.Suffix.BWT
  where

import Data.List (sort, foldl')
import qualified Data.Map as M

-- Generate list of all rotations of input string.
rotations :: String -> [String]
rotations xs = rotate (length xs) xs
  where
    rotate :: Int -> String -> [String]
    rotate 0 _  = []
    rotate n xs = [ys] ++ (rotate (n - 1) ys)
      where ys = (last xs) : (init xs)

-- Burrows-Wheeler matrix.
bwm :: String -> [String]
bwm = sort . rotations

-- Burrows-Wheeler transform.
-- last column of the BW matrix
bwtViaBwm :: String -> String
bwtViaBwm xs = foldr (\xs acc -> last xs : acc) [] (bwm xs)


-- Given BWT string, return B-ranks and counts
-- B-ranks: number of previous occurrences for each character 
--          in the BWT string
-- counts: map of the number of times a character appears
rankBwt :: String -> ([Int], M.Map Char Int)
rankBwt bs = foldl' f ([], M.empty) bs
  where
    f (ranks, counts) b = (ranks ++ [r], counts')
      where
        r = M.findWithDefault 0 b counts
        counts' = M.insert b (r + 1) counts

-- Given the character counts,
-- return map from character to the first row
-- of the BWM prefixed by the character.
-- Note: BWM is sorted by the first column, so
--       the same characters are always grouped together.
firstCol :: M.Map Char Int -> M.Map Char Int
firstCol counts = snd $ foldl' f (0, M.empty) sortedKeys
  where
    sortedKeys = M.keys counts
    f (n, firsts) k = (n + c, M.insert k n firsts)
      where c  = M.findWithDefault 0 k counts

-- Reverse Burrows-Wheeler transform.
reverseBwt bs = f "$" 0
  where
    f acc i = if c == '$' then acc else f (c : acc) (first + rank)
      where
        (ranks, counts) = rankBwt bs
        firsts = firstCol counts
        c = bs !! i
        first = (M.findWithDefault 0 c firsts)
        rank = ranks !! i


bwt = bwtViaBwm


tests = [ "Tomorrow_and_tomorrow_and_tomorrow$"
         , "It_was_the_best_of_times_it_was_the_worst_of_times$"
         , "in_the_jingle_jangle_morning_Ill_come_following_you$" ]

ans = [ "w$wwdd__nnoooaattTmmmrrrrrrooo__ooo"
      , "s$esttssfftteww_hhmmbootttt_ii__woeeaaressIi_______"
      , "u_gleeeengj_mlhl_nnnnt$nwj__lggIolo_iiiiarfcmylo_oo_" ]

check1 = map bwt tests == ans

check2 =
  tests == map (reverseBwt . bwt) tests

