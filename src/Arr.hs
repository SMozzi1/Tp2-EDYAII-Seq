{-
 Este módulo requiere la librería Vector. 
 
 Esta puede instalarse utilizando Cabal, ejecutando el siguiente código  
 en un intérprete de comandos: 
 
 $ cabal update
 $ cabal install vector
 
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Arr (Arr, length, tabulate, (!), subArray, fromList, flatten, empty) where

import Prelude hiding (length)
import qualified Data.Vector as V
import Control.Monad

newtype Arr a = A {getVector ::  V.Vector a }


length    :: Arr a -> Int -- O(1)
length (A p)  = V.length p


(!) :: Arr a -> Int -> a -- O(1)
(!) (A p) i = (V.!) p i


subArray :: Int ->  -- índice inicial -- O(1)
            Int ->  -- tamaño
            Arr a -> Arr a
subArray i n (A p) = A (V.slice i n p)


tabulate :: (Int -> a) -> Int -> Arr a
tabulate f n  = A (V.generate n f)


fromList :: [a] -> Arr a
fromList xs = A (V.fromList xs)


empty :: Arr a
empty = A (V.empty)


flatten :: Arr (Arr a) -> Arr a
flatten (A pa) = A (join (fmap getVector pa))


instance Show a => Show (Arr a) where
         show p = "<"++ show' p (length p) 0
            where show' p n i | i == n     = ">"
                              | i == (n-1) = show (p ! i) ++ ">"
                              | otherwise  = show (p ! i) ++ ","++ show' p n (i+1)
                              
instance Eq a => Eq (Arr a) where
         s == p = length s == length p && and [s ! i == p ! i | i <- [0..length s - 1]]
