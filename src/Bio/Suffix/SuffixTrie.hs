module Bio.Suffix.SuffixTrie
  where

import Data.Maybe
import qualified Data.Map as M


data SuffixTrie = Leaf | Node (M.Map Char SuffixTrie) 
    deriving (Show)

-- Empty suffix trie.
empty :: SuffixTrie
empty = Node M.empty

-- Insert a suffix into the suffix trie.
-- query string must be $-terminated
insert :: String -> SuffixTrie -> SuffixTrie
insert (x:xs) (Node children) = Node $ M.insert x subtrie children
  where 
    subtrie = case M.lookup x children of
        Just trie -> insert xs trie
        Nothing -> if x == '$' then Leaf else insert xs empty
insert _ tree = tree

-- TODO: move
-- Return list of suffixes.
suffixes :: String -> [String]
suffixes [] = []
suffixes xs = xs : (suffixes $ tail xs)

-- Create a suffix trie.
fromString :: String -> SuffixTrie
fromString x = foldr insert empty (suffixes t)
  where t = x ++ "$"

-- Return leaf or node at end of path given by string,
-- or nothing if path falls off.
followPath :: SuffixTrie -> String -> Maybe SuffixTrie
followPath trie (x:xs) = case trie of
    Node children ->
        case M.lookup x children of
            Just trie' -> followPath trie' xs
            Nothing -> Nothing
    Leaf -> Nothing
followPath trie s = case s of
    [] -> Just trie
    _  -> Nothing

-- Whether suffix trie has substring s.
hasSubstring :: SuffixTrie -> String -> Bool
hasSubstring t s = case followPath t s of
    Just _  -> True
    Nothing -> False

-- Whether trie has suffix s.
hasSuffix :: SuffixTrie -> String -> Bool
hasSuffix t s = case followPath t (s ++ "$") of
    Just Leaf -> True
    _ -> False

