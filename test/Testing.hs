-- | A stand-alone testing module for Regex parser. 

module Main where

import Parser          (parseReg,Regex(..))
import Regex           (match, match1)
import Text.Regex.TDFA ((=~))

import Test.QuickCheck

{-
Idea:

1. Generate a random AST of our Tokens
  Positive Testing: 
    - From the AST, generate a String that definitely matches this AST
  Naive testing (will result mostly in negative testing):
    - From this AST, construct a Haskell-style regular expression and see if our algorithm and
      the Haskell-implementation result in the same 'match'-output
    - generate a random String and run both on it
-}

genAST :: Gen Regex
genAST = frequency [(24, sized helper), (1,return Epsilon)]
  where 
    helper :: Int -> Gen Regex
    helper 0 = genLiteral
    helper n = frequency [
              (1, genOr n)
            , (1, genConcat n)
            , (3, genLiteral)
            , (1, genPlus n)
            , (1, genKleene n)
            , (1, genOptional n)
          ]

    genLiteral :: Gen Regex
    genLiteral = Literal <$> elements ['a'..'z']

    genPlus :: Int -> Gen Regex
    genPlus k = let k' = k - 1 in do
      a <- helper k'
      return $ Plus a

    genKleene :: Int -> Gen Regex
    genKleene k = let k' = k - 1 in do
      a <- helper k'
      return $ Kleene a

    genOptional :: Int -> Gen Regex
    genOptional k = let k' = k - 1 in do
      a <- helper k'
      return $ Optional a

    genConcat :: Int -> Gen Regex
    genConcat k =  let k' = k `div` 2 in do
      a <- helper k'
      b <- helper k'
      return (Concat a b)

    genOr :: Int -> Gen Regex
    genOr k =  let k' = k `div` 2 in do
      a <- helper k'
      b <- helper k'
      return (Or a b)

instance Arbitrary Regex where
  arbitrary = genAST

createRgx r = "^" ++ createRegex r ++ "$"

createRegex :: Regex -> String
createRegex Epsilon      = "()"
createRegex Dot          = "\\."
createRegex (Literal c)  = escape c
  where
    escape :: Char -> String
    escape c     = if c `elem` specialChars then "\\" ++ [c] else [c]
    specialChars = [
        '.'
      , '*'
      , '+'
      , '*'
      , '?'
      , '('
      , ')'
      , '['
      , ']'
      , '\\'
      , '^'
      , '$'
      ]
createRegex (Plus r)     = "(" ++ createRegex r ++ ")+"
createRegex (Kleene r)   = "(" ++ createRegex r ++ ")*"
createRegex (Optional r) = "(" ++ createRegex r ++ ")?"
createRegex (Concat a b) = createRegex a ++ createRegex b
createRegex (Or a b)     = "(" ++ createRegex a ++ "|" ++ createRegex b ++ ")"

-- TODO: use QuickCheck to randomly populate constructors instead of using
-- minimal implementations.
createMatch :: Regex -> String
createMatch Epsilon      = ""
createMatch Dot          = "a"
createMatch (Literal c)  = [c]
createMatch (Plus r)     = createMatch r -- minimal r+
createMatch (Kleene r)   = ""            -- minimal r*
createMatch (Optional r) = ""            -- minimal r?
createMatch (Concat a b) = createMatch a ++ createMatch b
createMatch (Or a _)     = createMatch a -- bias on `a`

prop_rgx_positive :: Regex -> Bool
prop_rgx_positive r = let 
  haskellRegex = createRgx r
  perfectMatch = createMatch r
  in match1 r perfectMatch == (fullMatch perfectMatch haskellRegex)

-- TODO: create more meaningful input strings => the input is meaningful even if its not human-readable
prop_rgx_negativish :: Regex -> String -> Bool
prop_rgx_negativish r input = let 
  haskellRegex = createRgx r
  in match1 r input == (fullMatch input haskellRegex)

fullMatch :: String -> String -> Bool
fullMatch input pattern =
    case input =~ pattern :: (Int, Int) of
        (0, len) -> len == length input
        _        -> False

main :: IO ()
main = do
  quickCheck prop_rgx_positive
  quickCheck prop_rgx_negativish
