{-#LANGUAGE BangPatterns #-}
module Main where

import System.Environment
import Data.Char
import Control.Monad
import Data.List

debug = True

main = do
    args <- getContents
    let lined = lines args
    case lined of
        [] -> do
            putStrLn $! "no arguments"
        (ps:params) -> do
            let n:: Int
                !n = read $! n_str
                !n_str = takeWhile (not . isSpace) $! ps
                k:: Int
                !k = read $! drop ((length n_str) + 1) ps
                is:: [Int]
                is = map read params
            when debug $! do 
                print n
                print k
                print is
            print $! calc n k is
    return ()


calc nlines k prices = 
    let maxPrices = getMaxPrices k prices
        salary = map calcSalary maxPrices
        sumSalary = foldl (+) 0 $! take k $! reverse $! sort salary
        ret = sumSalary
    in ret


--getMaxPrices:: Int-> [Int]-> [(MinPrice,MaxPrice)]
getMaxPrices k [] = []
getMaxPrices k is@(min0:_) = 
    let maxs = take k $! reverse $! sort is
        splitted = splitByMax maxs is
    in splitted


{- 
splitByMax:: [Int]-> [Int]-> [[Int]]
splitByMax maxs [] = []
splitByMax maxs is@(min0:_) = let (_, ret ) = foldl helper init is
    in reverse ret
    where
    --init:: ([Int],[[Int]])
    init = ( [] -- current range
           , [] -- list of ranges
           )
    
    helper (cur_range, ranges) x | any (== x) maxs = 
        ( [], (reverse (x:cur_range)):ranges)
    helper (cur_range, ranges) x = 
        ( x:cur_range, ranges)
-}


splitByMax:: [Int]-> [Int]-> [[Int]]
splitByMax maxs [] = []
splitByMax maxs is@(min0:_) = let (_, lastline, ret ) = foldl helper init is
    in reverse $! (reverse lastline):ret 
    where
    --init:: ([Int],[[Int]])
    init = ( min0
           , [] -- current range
           , [] -- list of ranges
           )
    
    helper ( curMax, cur_range, ranges) x | curMax < x = 
        ( x, x:cur_range, ranges)
    helper ( curMax, cur_range, ranges) x | curMax > x = 
        ( x, [x], (reverse cur_range):ranges)
    helper ( curMax, cur_range, ranges) x = 
        ( curMax, x:cur_range, ranges)
    
        
    -- helper (curMin, curMax, cur_range, ranges) x = 

calcSalary::[Int]-> Int
calcSalary [] = 0
calcSalary is = let sorted = sort is
    in last sorted - head sorted

