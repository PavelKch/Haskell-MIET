module Lists where

-- вектор задаётся списком координат
newtype Point = Point [Double] deriving (Eq, Show, Read)

-- distance x y находит расстояние между двумя точками в n-мерном
-- пространстве. Если число координат точек разное, сообщите об ошибке.
-- distance (Point [1.0, 0.0]) (Point [0.0, 1.0]) == sqrt 2.0
-- distance (Point [0.0, 0.0]) (Point [0.0, 1.0]) == 1.0

-- используйте рекурсию и сопоставление с образцом
distance :: Point -> Point -> Double
distance x y = sqrt (distanceHelp x y)

distanceHelp :: Point -> Point -> Double
distanceHelp (Point[]) (Point[]) = 0
distanceHelp (Point[]) _ = error " Dot dimensions do not match !"
distanceHelp _ (Point[]) = error " Dot dimensions do not match !"
distanceHelp (Point(x:xs)) (Point(y:ys)) = (x - y) ^ 2 + distanceHelp (Point xs) (Point ys)

-- intersect xs ys возвращает список, содержащий общие элементы двух списков.
-- intersect [1, 2, 4, 6] [5, 4, 2, 5, 7] == [2, 4] (или [4, 2]!)
-- intersect [1, 2, 4, 6] [3, 5, 7] == []

-- используйте рекурсию и сопоставление с образцом
intersect :: [Integer] -> [Integer] -> [Integer]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) ys = if  findElement x ys 
    then x : intersect xs ys
    else intersect xs ys

{- Функция поиска заданного элемента в списке, True если элемент встречается в списке, False если нет -}
findElement :: Integer -> [Integer] -> Bool
findElement x ([])   = False
findElement x (y:ys) = x == y || findElement x  ys

-- zipN принимает список списков и возвращает список, который состоит из
-- списка их первых элементов, списка их вторых элементов, и так далее.
-- zipN [[1, 2, 3], [4, 5, 6], [7, 8, 9]] == [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
-- zipN [[1, 2, 3], [4, 5], [6]] == [[1, 4, 6], [2, 5], [3]]
zipN :: [[a]] -> [[a]]
zipN [] = []
zipN [[]] = [[]]
--zipN [[]:[]] = [[]]
zipN xss  = ziphead xss : zipN (ziptail xss)

-- ziphead принимает список списков и возвращает список из первых элементов
ziphead :: [[a]] -> [a]
ziphead [] = [] 
ziphead [[]] = [] 
--ziphead [x] = head x
ziphead (xs:xss) = head xs : ziphead xss
 
-- ziptail принимает список списков и возвращает список из спсков без первых элементов
ziptail:: [[a]] -> [[a]]
ziptail [[]] = [[]] 
ziptail [] = [] 
ziptail (xs:xss) = tail xs : ziptail xss

-- Нижеперечисленные функции можно реализовать или рекурсивно, или с помощью
-- стандартных функций для работы со списками (map, filter и т.д.)
-- Попробуйте оба подхода! Хотя бы одну функцию реализуйте обоими способами.

-- Если в списке xs есть такие элементы x, для которых f x == True, то
-- find f xs возвращает Just (первый x), а findLast f xs -- Just (последний x).
-- Если таких нет, то обе функции возвращают Nothing
-- find (> 0) [-1, 2, -3, 4] == Just 2
-- findLast (> 0) [-1, 2, -3, 4] == Just 4
-- find (> 0) [-1, -2, -3] == Nothing
find, findLast :: (a -> Bool) -> [a] -> Maybe a
find f xs = if null ans 
    then Nothing 
    else Just (head ans)
        where ans = filter f xs
		
findLast f xs = find f (reverse xs)



-- mapFuncs принимает список функций fs и возвращает список результатов 
-- применения всех функций из fs к x.
-- mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 == [9, 4, 0]

mapFuncs :: [a -> b] -> a -> [b]
mapFuncs [] x = []
mapFuncs (f:fs) x = f x : mapFuncs fs x

mapFuncs' :: [a -> b] -> a -> [b]
mapFuncs' fs x' = map (\f -> f x') fs

-- satisfiesAll принимает список предикатов (функций, возвращающих Bool) preds
-- и возвращает True, если все они выполняются (т.е. возвращают True) для x.
-- Полезные стандартные функции: and, all.
-- satisfiesAll [even, \x -> x rem 5 == 0] 10 == True
-- satisfiesAll [] 4 == True (кстати, почему?)

satisfiesAll :: [a -> Bool] -> a -> Bool
satisfiesAll ps xs = all (==True) (map (\f -> f xs) ps)

-- Непустой список состоит из первого элемента (головы)
-- и обычного списка остальных элементов
-- Например, NEL 1 [2, 3] соотвествует списку [1, 2, 3], а NEL 1 [] -- списку [1].
data NEL a = NEL a [a] deriving (Show, Eq)

-- Запишите правильный тип (т.е. такой, чтобы функция имела результат для любых аргументов
-- без вызовов error) и реализуйте функции на NEL, аналогичные tail, last и zip
tailNel :: NEL a -> Maybe (NEL a)
tailNel (NEL _ []) = Nothing
tailNel (NEL _ x)  = Just (NEL (head x) (tail x))

lastNel :: NEL a -> a
lastNel (NEL x []) = x
lastNel (NEL _ x)  = last x

zipNel :: NEL a -> NEL b -> Maybe (NEL (a, b))
zipNel x1 x2 = listToNel ( zip (nelToList x1) (nelToList x2) )

listToNel :: [a] -> Maybe (NEL a)
listToNel []     = Nothing
listToNel (x:xs) = Just (NEL x xs) 

nelToList :: NEL a -> [a]
nelToList (NEL x []) = [x]
nelToList n@(NEL x xs) = x : nelToList( NEL (head xs) (tail xs))