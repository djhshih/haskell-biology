module Bio.Suffix.BWT
  where

import Data.List (sort)


-- Generate list of all rotations of input string
rotations :: String -> [String]
rotations xs = rotate (length xs) xs
  where
    rotate :: Int -> String -> [String]
    rotate 0 _  = []
    rotate n xs = [ys] ++ (rotate (n - 1) ys)
      where ys = (last xs) : (init xs)

-- Burrows-Wheeler matrix
bwm :: String -> [String]
bwm = sort . rotations

-- Burrow-Wheeler transform
bwt :: String -> String
bwt xs = foldr (\xs acc -> last xs : acc) [] (bwm xs)

test = [ bwt "Tomorrow_and_tomorrow_and_tomorrow$"
       , bwt "It_was_the_best_of_times_it_was_the_worst_of_times$"
       , bwt "in_the_jingle_jangle_morning_Ill_come_following_you$" ]

ans = [ "w$wwdd__nnoooaattTmmmrrrrrrooo__ooo"
      , "s$esttssfftteww_hhmmbootttt_ii__woeeaaressIi_______"
      , "u_gleeeengj_mlhl_nnnnt$nwj__lggIolo_iiiiarfcmylo_oo_" ]

