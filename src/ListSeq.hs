module ListSeq where

import Seq
import Par


instance Seq [] where
    emptyS        = []
    singletonS x  = [x]
    lengthS       = length                  --Prelude
    nthS s i      = s !! i
    tabulateS f n = map f [0 .. n - 1]
    mapS = map                              --Prelude
    filterS = filter                        --Prelude
    appendS = (++)
    takeS s n = take n s                            --Prelude
    dropS s n = drop n s                            --Prelude

    showtS [] = EMPTY
    showtS [x] = ELT x
    showtS s = NODE (takeS s mid) (dropS s mid)     --chequear si tienen que ser recurisvas (*)
        where mid = lengthS s `div` 2

    showlS [] = NIL
    showlS (x:xs) = CONS x xs

    joinS [] = []
    joinS (xs:xss) = xs `appendS` joinS xss


    fromList = id                           --Prelude


{-
Alvaro:
El tema de showtS:
    no se s

-}