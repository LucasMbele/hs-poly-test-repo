module Part3.Tasks where
import Data.List (nub, maximumBy)
import Data.Function (on)
import Util (notImplementedYet)


-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = [(f n)] ++ (finc f (n+1))


-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq [] = Nothing
mostFreq lst = notImplementedYet

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq lst = nextElem : uniq lstWithoutRepetition
   where
    nextElem = head lst
    tailLst = tail lst
    lstWithoutRepetition = filter (/= nextElem) tailLst

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l =  k
              uniq_list = uniq k in
              map (\k -> (k, filter (\v -> f(v) == k) l)) uniq_list
              where
                k = map (f) l