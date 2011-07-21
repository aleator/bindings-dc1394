{-#LANGUAGE QuasiQuotes#-}
module Main where

import Control.Applicative
import Data.String.Interpolation
import System.Environment
import Data.Char 

main = do
    [a, d] <- getArgs
    l <- map (drop 5) . lines <$> getContents
    let typeName = (up . toCamel) . take (read d) $ dropI $ head $ l
    let constructors = map (a++) . map (drop (read d+1)) $ map dropI $ l
    putStrLn $ toEnu typeName  constructors $ l
    putStrLn $ tofrom typeName constructors $ l
    putStrLn $ toto typeName   constructors $ l

dropI = tail . dropWhile (/='_')
takeI = takeWhile (/='_')

splitTypeName d s = (take d s, drop (d+1) s) 

toCamel ('_':x:xs) = toUpper x:toCamel xs
toCamel (x:xs) = toLower x:toCamel xs
toCamel [] = []

up (x:xs) = toUpper x:xs
up x = x

toEnu n xs origs = [str|
data $n$ = $head xs$ 
#c in tail xs:     $bar$ $c$ | $endline$#
      deriving (Show,Eq)|]
    where bar = "|"

tofrom n xs origs = [str|
#(c,o) in zip xs origs:from$n$ $c$ = c'$o$ | $endline$#|]

toto n xs origs= [str|
to$n$ i 
#(c,o) in zip xs origs:  $bar$ i == $c$ = c'$o$ | $endline$#|]
    where bar = "|"
