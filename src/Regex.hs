module Regex
    ( match           -- * regex on string input
    , matchWithTrace  -- * regex on string input with trace
    , checkWithTrace  -- * regex (DFA) on string input with trace (internal)
    ) where

import Datatypes
import qualified DFA (fromNFAMulti, flattenToDFA, fromNFA)
import qualified NFA (epsilonClosure, fromRegex)

import qualified DMap
import qualified RDSL as AST
import qualified Data.Map as Map
import qualified Control.Monad.State as S

-- | Match a @Regex@ datatype against a string input.
match :: AST.Regex -> String -> Bool
match pattern input = let dfa = (DFA.fromNFA . NFA.fromRegex) pattern in 
    check dfa input

-- | Contains: an accepted/rejected status for the full matching,
--             and a trace with a status for each state of the traversal
--             step-by-step.
type MatchWithTrace = (Bool,[(State, Bool)]) 

matchWithTrace :: AST.Regex -> String -> MatchWithTrace
matchWithTrace pattern input = let dfa = (DFA.fromNFA . NFA.fromRegex) pattern in 
    checkWithTrace dfa input

-- Match a Regex (i.e., a DFA) against a string input.
check :: DFA -> String -> Bool
check dfa@(DFA start accepts ts) = go start
    where
        go :: State -> String -> Bool
        go current []     = current `elem` accepts
        go current (c:cs) = any proceedWithChar ['.', c] -- lazy
            where 
                proceedWithChar :: Char -> Bool
                proceedWithChar c = case Map.lookup c $ DMap.lookup current ts of
                    Nothing        -> False
                    Just nextState -> go nextState cs

-- Works the same as @check@, but at each step of the traversal marks the state.
checkWithTrace :: DFA -> String -> MatchWithTrace
checkWithTrace dfa@(DFA start accepts ts) input =
    let (matched, state) = S.runState (go start input) (False, []) in
        (matched, reverse $ snd state)
    where
        go :: State -> String -> S.State MatchWithTrace Bool
        go current []     = do
            let isAccepting = current `elem` accepts
            -- The current state verifies the input, thus set the FoundFlag to True
            -- to avoid adding further trace
            S.modify (\(_, trace) -> if isAccepting then (True,(current,True):trace) else (False,trace))
            return isAccepting
        go current (c:cs) = do
            let literalsForCurrent = Map.keys $ DMap.lookup current ts
            -- If the next state has a dot transition, consider both that path
            -- and the literal path
            matched <- mapM proceedWithChar $
                if '.' `elem` literalsForCurrent then ['.', c] else [c]
            return $ or matched
            where 
                proceedWithChar :: Char -> S.State MatchWithTrace Bool
                proceedWithChar c = case Map.lookup c $ DMap.lookup current ts of
                    Nothing        -> do
                        -- Add the current state as 'faulty', retain the found flag
                        S.modify (\s@(found, trace) -> if found then s else (False,(current,False):trace))
                        return False
                    Just nextState -> do
                        -- Add the current state as valid on the verification path, retain the found flag
                        S.modify (\s@(found, trace) -> if found then s else (False,(current,True):trace))
                        go nextState cs
