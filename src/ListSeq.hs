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
    showtS s = NODE (takeS s mid) (dropS s mid)
        where mid = lengthS s `div` 2
        
    showlS [] = NIL
    showlS (x:xs) = CONS x xs

    joinS [] = []
    joinS (xs:xss) = xs `appendS` joinS xss

    reduceS f b [] = b
    reduceS f b [x] = f b x
    reduceS f b xs = reduceS f b (comprimir xs)
        where 
            comprimir [] = []
            comprimir [x] = [x]
            comprimir (x:y:xs) = f x y : comprimir xs

    scanS f b [] = ([], b)
    scanS f b [x] = ([b], f b x)
    scanS f b xs = 
        let xs' = comprimir xs
            (s', total) = scanS f b xs'
            n = length xs
            r = expandir s' xs
                in (r, total)
        where
            comprimir [] = []
            comprimir [x] = [x]
            comprimir (x:y:xs) = f x y : comprimir xs

            expandir [] _ = []
            expandir (p:ps) [] = [p]
            expandir (p:ps) (x:y:xs) = p : f p x : expandir ps xs


    fromList = id                           --Prelude
{-
Alva:
El tema de showtS:
    no se si showtS y showlS tienen que ser recursivos, es decir:
        Si al hacer showtS [1,2,3,4,5,6], devuelve NODE [1,2,3] [4,5,6]
        Ese resultado es correcto , o deberia hacer un arbolito con todas hojas :p

INCISO B)

filterS(): W = O(|s| + Σ W(f sᵢ))
           S = O(|s| + max S(f sᵢ))

reduceS(): W = O(|s|)
           S = O(|s|)

scanS(): W = O(|s|)
           S = O(|s|)


-}