module SuffixTree
  where

import Data.Maybe
import qualified Data.Map as M

-- TODO replace string with (offset, length)

-- Each node consist of a label and a map of outgoing edges to children
-- the label represents the label of the incoming edge.
data SuffixTree = Leaf String | Node String (M.Map Char SuffixTree)
    deriving (Show)

label :: SuffixTree -> String
label (Leaf l) = l
label (Node l _) = l

-- Create an empty suffix tree.
empty :: SuffixTree
empty = Leaf ""

-- query string must be $-terminated
insert :: String -> SuffixTree -> SuffixTree
insert [] tree = tree
insert s@(x:xs) (Node lab ch) = Node lab (M.insert x subtree ch)
  where
    subtree = case M.lookup x ch of
        -- fell off the tree: make new edge hanging off current node
        Nothing -> Leaf s
        -- descend down the tree
        Just tree ->
            let l = label tree
            in case match s l of
                -- label is exhausted: recurse with remainder of string s
                (_, sr, []) -> insert sr tree
                -- fell off in middle of label: insert new node bisecting edge
                (matched, sr, lr) -> Node matched c'
                  where
                    -- insert new child node with new substring
                    -- and original node/leaf with curtailed label
                    c' = M.insert (head sr) (Leaf sr) $
                         M.insert (head lr) tree' M.empty
                    tree' = case tree of
                        Leaf l -> Leaf lr
                        Node l c -> Node lr c
insert s t@(Leaf lab) = case s of
  "$" -> t
  -- if inserting into a leaf, turn the leaf into a node
  _   -> insert s (Node lab M.empty)

-- TODO: move
-- Return list of suffixes
suffixes :: String -> [String]
suffixes [] = []
suffixes xs = xs : (suffixes $ tail xs)

-- Create a suffix tree.
fromString :: String -> SuffixTree
fromString x = foldr insert empty (suffixes t)
  where t = x ++ "$"

-- Match two strings.
-- Return tuple (matched, remainder of s1, remainder of s2).
match :: String -> String -> (String, String, String)
match s1 s2 = match' s1 s2 []
  where
    match' [] s2 matched = (matched, [], s2)
    match' s1 [] matched = (matched, s1, [])
    match' s1@(x:xs) s2@(y:ys) matched
        | x == y     =  match' xs ys (matched ++ [x])
        | otherwise  =  (matched, s1, s2)

-- Follow path given by string.
-- Return the leaf or node at the end of the path
-- or nothing if path falls off.
followPath :: SuffixTree -> String -> Maybe SuffixTree
followPath tree [] = Just tree
followPath tree s = 
    let l = label tree
    in case match s l of
        -- both query string and label are exhausted
        (_, [], []) -> Just tree
        -- only query string is exhausted: return a new node bisecting
        -- the edge along which the query string matched
        (matched, [], lr) ->
            Just $ Node matched (M.insert (head lr) (Leaf lr) M.empty)
        -- only label is exhausted: descend
        (_, sr@(x:xs), []) ->
            case tree of
                Node _ c ->
                    case M.lookup x c of
                        Just tree' -> followPath tree' sr
                        Nothing -> Nothing
                Leaf _ -> Nothing
        -- neither is exhausted: path fell off
        otherwise -> Nothing

-- Whether suffix tree has substring s.
hasSubstring t s = case followPath t s of
    Just _  -> True
    Nothing -> False

-- Whether suffix tree has substring s.
hasSuffix :: SuffixTree -> String -> Bool
hasSuffix t s = case followPath t (s ++ "$") of
    Just (Leaf _) -> True
    _ -> False

-- test that all suffixes of t are in suffix tree of t
test =
    let t = "abaaba"
        tree = fromString t 
    in all (hasSuffix tree) (suffixes t)

