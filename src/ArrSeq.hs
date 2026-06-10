{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module ArrSeq where

import Par
import Seq
import qualified Arr as A
import Arr ((!))

instance Seq A.Arr where
    emptyS         = A.empty
    singletonS x   = A.tabulate (const x) 1
    lengthS arr    = A.length arr
    nthS arr i     = arr ! i
    tabulateS f n  = A.tabulate f n
    mapS f arr     = A.tabulate (\i -> f(nthS arr i))(A.length arr)
    filterS p arr  = joinS (mapS (\x -> if p x then singletonS x else emptyS) arr)
    appendS a1 a2  = tabulateS (\i -> if i < lengthS a1 then a1!i else a2!(i - lengthS a1)) (lengthS a1 + lengthS a2)
    joinS arr      = A.flatten arr
    takeS arr n    = A.subArray 0 n arr
    dropS arr n    = A.subArray n (lengthS arr - n) arr

    showtS arr     | n == 0 = EMPTY
                   | n == 1 = ELT (arr!0)
                   | otherwise = let (l,r) = arrL ||| arrR
                                 in NODE l r
                   where n = lengthS arr
                         arrL = A.subArray 0 ((n+1) `div` 2) arr
                         arrR = A.subArray ((n+1) `div` 2) (n `div` 2) arr

    showlS arr     | n == 0 = NIL
                   | n == 1 = CONS (arr!0) emptyS
                   | otherwise = CONS (arr!0) (dropS arr 1)
                   where n = lengthS arr
    -- reduceS     =
    -- scanS       =
    fromList       = A.fromList