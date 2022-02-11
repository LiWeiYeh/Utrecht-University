module Algebra where

import Model
import Data.List

-- Exercise 5 
type ProgramAlgebra pr r c d a pat
    = ([r] -> pr,                   -- program
       Model.Ident -> [c] -> r,     -- rule
       c,                           -- go
       c,                           -- take
       c,                           -- mark
       c,                           -- nothing
       d -> c,                      -- turn
       d -> [a] -> c,               -- case
       Ident -> c,                  -- ident
       pat -> [c] -> a)             -- alt

foldProgram :: ProgramAlgebra pr r c Direction a Pat -> Program -> pr
foldProgram (program, rule, go, take, mark, nothing, turn, case', ident, alt) = fpr
    where
        fpr (Program r)     = program (map fr r)
        fr  (Rule id cmds)  = rule id (map fc cmds)
        fc   Go             = go 
        fc   Take           = take
        fc   Mark           = mark
        fc   Nothing'       = nothing
        fc  (Turn dir)      = turn dir
        fc  (Case dir alts) = case' dir (map fa alts)
        fc  (Ident id)      = ident id
        fa  (Alt pat cmds)  = alt pat (map fc cmds)

-- Exercise 6

checkProgram :: Program -> Bool
checkProgram program = hasNoCallsToUndefinedRules program && 
                       hasRuleStart program &&
                       hasNoDoubleDefinedRules program &&
                       hasNoPatternMatchFailure program
    where
        hasNoCallsToUndefinedRules :: Program -> Bool
        hasNoCallsToUndefinedRules =
            foldProgram hasNoCallsToUndefinedRules
            where
                -- converts all cmds without ident into an empty list
                -- concatenates all cases where there is an ident
                -- end up with rules that contain [idents, calls (which are also idents)]
                -- checks if for all elements in the calls, the element is available in the idents
                hasNoCallsToUndefinedRules :: ProgramAlgebra Bool (Ident, [Ident]) [Ident] Direction [Ident] Pat
                hasNoCallsToUndefinedRules =
                    (\rs -> let (ids, calls) = (unzip rs)
                            in checkCallInIdent ids (concat calls),    -- program
                     \id cmds -> (id, concat cmds),                    -- rule
                     [],                                               -- go
                     [],                                               -- take
                     [],                                               -- mark
                     [],                                               -- nothing
                     \dir -> [],                                       -- turn
                     \dir alts -> concat alts,                         -- case
                     \id -> [id],                                      -- ident
                     \pat cmds -> concat cmds)                         -- alt

                checkCallInIdent :: [Ident] -> [Ident] -> Bool
                checkCallInIdent ids = all ((== True) . (\ x -> x `elem` ids))

        hasRuleStart :: Program -> Bool
        hasRuleStart = 
            foldProgram hasRuleStartAlgebra
            where
                -- checks if any ident matches with "start"
                -- if so, it should be True, so you can find it with the or function
                hasRuleStartAlgebra :: ProgramAlgebra Bool Bool () Direction () Pat
                hasRuleStartAlgebra =
                    (or,                                    -- program
                     \id _ -> id == "start",                -- rule
                     (),                                    -- go
                     (),                                    -- take
                     (),                                    -- mark
                     (),                                    -- nothing
                     \_ -> (),                              -- turn
                     \_ _ -> (),                            -- case
                     \_ -> (),                              -- ident
                     \_ _ -> ())                            -- alt

        hasNoDoubleDefinedRules :: Program -> Bool
        hasNoDoubleDefinedRules = 
            foldProgram hasNoDoubleDefinedRulesAlgebra
            where
                -- gets only the ident of the rule
                -- from the list of rules, check if the length is equal
                -- to the length with removed duplicates
                hasNoDoubleDefinedRulesAlgebra :: ProgramAlgebra Bool Ident () Direction () Pat
                hasNoDoubleDefinedRulesAlgebra =
                    (\rs -> (length (nub rs)) == length rs, -- program
                     \id _ -> id,                           -- rule
                     (),                                    -- go
                     (),                                    -- take
                     (),                                    -- mark
                     (),                                    -- nothing
                     \_ -> (),                              -- turn
                     \_ _ -> (),                            -- case
                     \_ -> (),                              -- ident
                     \_ _ -> ())                            -- alt

        hasNoPatternMatchFailure :: Program -> Bool
        hasNoPatternMatchFailure = 
            foldProgram hasNoPatternMatchFailureAlgebra
            where
                -- for the case, check if the case either has an _, or it
                -- contains all the 5 possibilities
                hasNoPatternMatchFailureAlgebra :: ProgramAlgebra Bool Bool Bool Direction Pat Pat
                hasNoPatternMatchFailureAlgebra =
                    (\rs -> all (==True) rs,                -- program
                     \id cmds -> all (==True) cmds,         -- rule
                     True,                                  -- go
                     True,                                  -- take
                     True,                                  -- mark
                     True,                                  -- nothing
                     \_ -> True,                            -- turn
                     \_ alts -> checkAllPats alts,          -- case
                     \_ -> True,                            -- ident
                     \pat cmds -> pat)                      -- alt

                checkAllPats :: [Pat] -> Bool
                checkAllPats pats = Underscore `elem` pats ||
                                    (Empty     `elem` pats && 
                                     Lambda    `elem` pats && 
                                     Debris    `elem` pats &&
                                     Asteroid  `elem` pats && 
                                     Boundary     `elem` pats)
