module Part1.Tasks where

import Data.Fixed
import Data.List
import Util(notImplementedYet)

fac :: Double -> Double
fac n =
 | n <= 1 = 1
 | otherwise = n * fac(n-1)


-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin = notImplementedYet

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos = notImplementedYet

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD x y
      | y == 0   = abs x
      | x == 0   = abs y
      | otherwise = myGCD y (x `mod` y)

isFebruaryCorrect :: Integer -> Integer -> Bool
isFebruaryCorrect day year
  | day < 29 = True
  | day == 29 && isLeapYear year = True
  | otherwise = False

isLeapYear :: Int -> Bool
isLeapYear y = y `mod` 4 == 0 && y `mod` 100 /= 0 || y `mod` 400 == 0


-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
      | year < minValidYear = False
      | month < 1 || month > 12 = False
      | day < 1 = False
      | month `elem` bigMonths && day <= 31 = True
      | month `elem` commonMonths && day <= 30 = True
      | month == 2 = isFebruaryCorrect day year
      | otherwise = False
      where
        minValidYear = 1970
        bigMonths = [1, 3, 5, 7, 8, 10, 12]
        commonMonths = [4, 6, 9, 7, 11]

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x y
      | y  == 0   = 1
      | y  == 1   = x
      | otherwise x * myPow x (y-1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x
        | (length [x | x <- [2..n-1],mod n x ==0]) > 0 = False
        | otherwise = True

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea points = 0.5 * abs (gaussLeft - gaussRight)
  where
    gaussLeft = gaussSeq points fst snd
    gaussRight = gaussSeq points snd fst

gaussSeq :: [Point2D] -> (Point2D -> Double) -> (Point2D -> Double) -> Double
gaussSeq points f1 f2 = f1 (points !! (n - 1)) * f2 (points !! 0) + gaussSeq' 0
  where
    n = length points
    gaussSeq' i
      | i == n - 1 = 0
      | otherwise = f1 (points !! i) * f2 (points !! (i + 1)) + gaussSeq' (i + 1)

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник

square :: Integer -> Integer
square x = x*x

triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c = if (a + b > c && a + c > b && b + c > a)
                                               if square c = square b + square a then 2
                                               else if square c < square b + square a then 1
                                               else 0
                       else -1


