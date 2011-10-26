{-# LANGUAGE QuasiQuotes #-}
module BayesSandbox where

import BayesDSL

[defVars| a, b, c, d, e |]
bn = mkBN [ a :-> b :-> e
          , a :-> d :-> e
          , c :-> d
          , a :-> e            
          ]     
test = map (check bn)
       [ c .|. e |: [ a ]
       , b .|. d |: [ c, e ]
       , a .|. c |: [ e ]
       , a .|. c |: [ b ]
       ]
testB = map (check bnB)
        [ b .|. c |: [] 
        , b .|. c |: [ d ] 
        , b .|. c |: [ a ]
        , b .|. c |: [ a, d] ]
bnB = mkBN [ a :-> b :-> d
           , a :-> c :-> d ] 