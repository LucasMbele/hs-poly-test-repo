module Part5.Tasks where

--import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f z [] = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z []  = z
myFoldr f z (x:xs) = x `f` myFoldr f z xs
-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (\x acc -> (f x) : acc) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldr ((++) . f) []

myConcat :: [[a]] -> [a]
myConcat lst = myFoldr (++) [] lst

myReverse :: [a] -> [a]
myReverse lst = myFoldl reverse [] lst
   where
       reverse n l = l : n

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr (\x acc -> if p x then x : acc else acc) []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p lst = myFoldr toPartition ([], []) lst
    where
      toPartition n res = if p n then (n : fst res, snd res) else (fst res, n : snd res)


