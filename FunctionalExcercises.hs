-- Author: Anna Roodbergen
--
-- Created April 23, 2018
--
-- Solutions for the Functional Language Excercises

module FunctionalExcercises where

import TestSuiteSupportModule

  -- A test suite containing multiple tests
excercisesTestSuite = TestSuite "Test functional excerises"
         [
         Test "item member of list" (member 2 [1, 2, 3] == True),
         Test "item not member of list" (member 4 [1, 2, 3] == False),

            --test cases for deleteItem
            Test "item not in list to delete" (deleteItem 1 [2, 3, 4] == [2, 3, 4]),
            Test "item deleted from list" (deleteItem 2 [1, 2, 3, 2] == [1, 3]),

            --test cases for deleteList
            Test "deleteList original list empty" (deleteList [[]] [1,2] == [[]]),
            Test "deleteList comparing list empty" (deleteList [[1,2],[1,3],[1,2]] [] == [[1,2],[1,3],[1,2]]),
            Test "list not in list" (deleteList [[1, 2], [1, 3], [1, 2]] [1, 4] == [[1, 2], [1, 3], [1, 2]]),
            Test "list found in list and deleted" (deleteList [[1, 2], [1, 3], [1, 2]] [1, 2] == [[1, 3]]),

            --test cases for removeAll
            Test "no matches to remove" (removeAll [1,2,3] [] == [1,2,3]),
            Test "no items to remove" (removeAll [] [1,2] == []),
            Test "items to remove not in list" (removeAll [1,3,4,6] [2,5] == [1,3,4,6]),
            Test "items removed from list" (removeAll [1,2,3,3,4,5] [3,4] == [1,2,5]),

            --test cases for deletelast
            Test "empty list to delete last" (deleteLast [] == []),
            Test "list with one item deletes last" (deleteLast [1] == []),
            Test "deletes last item of list" (deleteLast [1, 2, 3, 4] == [1, 2, 3]),

            --test cases for reverseList
            --Test "reversing empty list returns empty" (reverseList [] = []),
            Test "reverses list of one item" (reverseList [1] == [1]),
            Test "reverses a list of items" (reverseList [1,2,3,4,5] == [5,4,3,2,1]),

            --test cases for minMaxFinder
            --Test "mins and maxs of empty set don't exist" (minMaxFinder [] = []),
            Test "min maxes for one item" (minMaxFinder [1] == [1,1]),
            Test "min maxes of a list" (minMaxFinder [2,5,4,6,1] == [1,6])
         ]

  --import Data.List

-- Problem 1
member :: (Eq a) => a -> [a] -> Bool
member a [] = False
member a (x:xs)
    | a == x  = True
    | otherwise = member a xs

--Problem 2
deleteItem :: (Eq a) => a -> [a] -> [a]
deleteItem x xs = filter(/=x) xs

--Problem 3
deleteList :: (Eq a) => [[a]] -> [a] -> [[a]]
deleteList [[]] xs = [[]]
deleteList yss [] = yss
deleteList yss xs = filter(/=xs) yss

--Problem 4
removeAll :: (Eq a) => [a] -> [a] -> [a]
removeAll [] _ = []
removeAll xs [] = xs
removeAll xs (y:ys) = removeAll (deleteItem y xs) ys

--Problem 5
deleteLast :: [a] -> [a]
deleteLast [] = []
deleteLast [x] = []
deleteLast (x:xs) = x : deleteLast xs

--Problem 6
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x] --recursion

--Problem 7
minMaxFinder :: (Ord a) => [a] -> [a]
minMaxFinder [] = []
minMaxFinder xs = [minimum xs, maximum xs]
