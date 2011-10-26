{-# LANGUAGE TypeOperators, TemplateHaskell, QuasiQuotes #-}
module BayesDSL where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Data.Map as M
import Data.Graph.Inductive
import Data.List
import Data.List.Split
import Data.Char

(!) = (M.!)

data Expr = Var String
          | Expr `Indep` Expr
          | Expr `Given` [Expr]
          | Expr :-> Expr

varName (Var nm) = nm

instance Show Expr where
  show (Var nm)       = nm
  show (a `Indep` b)  = show a ++ " _|_ " ++ show b
  show (a `Given` bs) = show a ++ " | "   ++ intercalate "," (map show bs)
  show (a :-> b)      = show a ++ " -> "  ++ show b
                    
defVars = QuasiQuoter e e e (nmsToDec . splitOn "," . filter (not . isSpace))
  where nmsToDec = return . map (\nm -> ValD (VarP $ mkName nm) (nmToLitB nm) [])
        nmToLitB = NormalB . AppE (ConE $ mkName "Var") . LitE . StringL
        e = error "Unsupported context!"

getVars expr = case expr of
  Var nm       -> [Var nm]  
  a `Indep` b  -> getVars a ++ getVars b
  a `Given` bs -> getVars a ++ concatMap getVars bs
  a :-> b      -> getVars a ++ getVars b

a .|. b = a `Indep` b
a |: bs = a `Given` bs

-- Bayes-Network
data BN label = BN { metadata :: M.Map label Node
                   , graph    :: UGr
                   }

mkBN exprs = BN hashMap $ mkGraph nodes (nub edges)
  where hashMap  = M.fromList $ zip varNames [1..]
        varNames = nub (concat varLists)
        varLists = map (map varName . getVars) exprs
        nodes    = [ (hashMap ! k, ()) | k <- varNames ]
        edges    = [ (hashMap ! k1, hashMap ! k2, ()) 
                   | vs <- varLists, k1:k2:_ <- tails vs ]
                  
bball bn nmA nmB nmsGiven = walk (md ! nmA) dirOut False []
  where walk cur dir rep steps
          | cur == end                  = True
          | not rep && cur `elem` given = dir && walk cur dirOut True steps     
          | otherwise                   = 
            or $ [ walk new dirIn  False $ (cur,new):steps | new <- sucs ]
              ++ [ walk new dirOut False $ (cur,new):steps | new <- pres ]
          where sucs = filterNodes $ suc gr cur
                pres | dir == dirOut = filterNodes $ pre gr cur                        
                     | otherwise     = []
                filterNodes = filter (not . (`elem` steps) . (,) cur)
        dirIn  = True
        dirOut = False
        gr     = graph    bn
        md     = metadata bn
        end    = md ! nmB
        given  = map (md !) nmsGiven

-- check expr
check bn (a `Given` bs) = case a of
  x `Indep` y -> not $ bball bn (varName x) (varName y) $ map varName bs
  otherwise   -> False
check _ _ = False


