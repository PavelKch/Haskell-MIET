-- Не забудьте добавить тесты.

module Poly where

-- Многочлены
-- a -- тип коэффициентов, список начинается со свободного члена.
-- Бонус: при решении следующих заданий подумайте, какие стали бы проще или
-- сложнее при обратном порядке коэффициентов (и добавьте комментарий).
{- Комманетарий, в задании 4 было проще при обратном порядке коэффициентов, если желаемый вывода строки в порядке уменьшения степени полинома.
Умножение было бы тоже проще реализовать при обратном порядке, если мы рекусивно отнимаем слева коэффциент, на следующем шаге следующий коэффициент снижает стпень на один -}
newtype Poly a = P [a]

-- Задание 1 -----------------------------------------

-- Определите многочлен $x$.
x :: Num a => Poly a
x = P [0, 1]

-- Задание 2 -----------------------------------------

-- Функция, считающая значение многочлена в точке
applyPoly :: Num a => Poly a -> a -> a
applyPoly (P []) _ = 0
applyPoly (P (c:cs)) x = c + x * applyPoly (P cs) x

-- Задание 3 ----------------------------------------

-- Определите равенство многочленов
-- Заметьте, что многочлены с разными списками коэффициентов
-- могут быть равны! Подумайте, почему.
instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P []) (P []) = True
    (==) (P []) (P (c2:cs2)) = c2 == 0
    (==) (P (c1:cs1)) (P []) = c1 == 0
    (==) (P [c1]) (P []) = c1 == 0
    (==) (P []) (P [c2]) = c2 == 0
    (==) (P []) _ = False
    (==) _ (P []) = False
    (==) (P (c1:cs1)) (P (c2:cs2)) = c1 == c2 && P cs1 == P cs2
 
-- Задание 4 -----------------------------------------
-- Определите перевод многочлена в строку. 
-- Это должна быть стандартная математическая запись, 
-- например: show (3 * x * x + 1) == "3 * x^2 + 1").
-- (* и + для многочленов можно будет использовать после задания 6.)
instance (Eq a, Num a, Show a) => Show (Poly a) where
    show (P []) = "0"
    show (P cs) = let 
        showTerm deg coeff = 
            let coeff' = if coeff == 1 && deg /= 0 then "" else show coeff
                deg' = if deg <= 1 then "x" else "x^" ++ show deg
            in if deg == 0 then coeff' else coeff' ++ deg'
        showTerms [] = ""
        showTerms [(deg,coeff)] = showTerm deg coeff
        showTerms ((deg,coeff):rest) =
            case showTerms rest of
                "" -> showTerm deg coeff
                otherTerms -> showTerm deg coeff ++ " + " ++ otherTerms
        in showTerms $ reverse $ filter (\(deg,coeff) -> coeff /= 0) $ zip [0..] cs
-- Задание 5 -----------------------------------------

-- Определите сложение многочленов
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P []) q = q
plus p (P []) = p
plus (P (c1:cs1)) (P (c2:cs2)) = P (c1 + c2 : coeffs)
    where P coeffs = plus (P cs1) (P cs2)

-- Задание 6 -----------------------------------------
-- Определите умножение многочленов
times :: Num a => Poly a -> Poly a -> Poly a
times (P []) _ = P []
times (P [c]) q = P (helper c q)
times (P (c1:cs1)) (P (c2:cs2)) = times (P [c1]) (P (c2:cs2)) + times (P cs1) (P (0:c2:cs2))

{- Функция умножения полинома на число -}
helper a (P [])  = []
helper a (P (c:cs))  = a * c : helper a (P cs)

-- Задание 7 -----------------------------------------

-- Сделайте многочлены числовым типом
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = (*) (P [-1])
    fromInteger n = P [fromInteger n]
    -- Эти функции оставить как undefined, поскольку для 
    -- многочленов они не имеют математического смысла
    abs    = undefined
    signum = undefined

-- Задание 8 -----------------------------------------

-- Реализуйте nderiv через deriv
class Num a => Differentiable a where
    -- взятие производной
    deriv  :: a -> a
    -- взятие n-ной производной
    nderiv :: Int -> a -> a
    nderiv n f = nderiv (n-1) (deriv f)

-- Задание 9 -----------------------------------------

-- Определите экземпляр класса типов
instance (Num a, Enum a) => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P [_]) = P []
    deriv (P (_:cs)) = P (zipWith (*) [1..] cs)