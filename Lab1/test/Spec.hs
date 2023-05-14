import FirstSteps
import Lists
import Luhn
import Test.Hspec
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "first steps" $ do
	-- Можно вложить глубже: describe "xor" do $ ... чтобы дать названия отдельным тестам
        it "xor" $ do
            xor True  True  `shouldBe` False
            xor True  False `shouldBe` True
            xor False True  `shouldBe` True
            xor False False `shouldBe` False
        it "max3" $ do
            max3  1  2  3 `shouldBe` 3
            max3  4  6  5 `shouldBe` 6
            max3  9  7  8 `shouldBe` 9
            max3 10 11 11 `shouldBe` 11
            max3 13 12 13 `shouldBe` 13
            max3 15 15 14 `shouldBe` 15
            max3 16 16 16 `shouldBe` 16
        it "median3" $ do
            median3  1  2  3 `shouldBe` 2
            median3  4  6  5 `shouldBe` 5
            median3  8  7  9 `shouldBe` 8
            median3 10 11 11 `shouldBe` 11
            median3 13 12 13 `shouldBe` 13
            median3 15 15 14 `shouldBe` 15
            median3 16 16 16 `shouldBe` 16
		
        it "geomProgression" $ do
            geomProgression 3.0 2.0 2 `shouldBe` 12.0
            --geomProgression 3.0 2.0 (-5) `shouldBe` error "The value of n must be greater than 0 !" -- с эти тестом проходит компиляцию, но тест проваливается ???
            evaluate (geomProgression 3.0 2.0 (-5)) `shouldThrow` anyException
            geomProgression 3.0 2.0 0 `shouldBe` 3.0
            geomProgression 1.0 1.0 100000 `shouldBe` 1.0
            geomProgression 1.0 0.5 10 `shouldBe` 9.765625e-4 --  1.0 / (2 ^ 10) == 9.765625e-4
            geomProgression 1.0 2.0 40 `shouldBe` 1.099511627776e12 --  2^40 == 1099511627776
            geomProgression 1.0 0.9 100 `shouldBe` 2.6561398887587544e-5
            geomProgression (10.0 ^ 100) 0.1 100 `shouldBe` 1.0000000000000056
			--geomProgression 3.0 2.0 (-5) `shouldBe` error "The value of n must be greater than 0 !" -- с эти тестом проходит компиляцию, но тест проваливается ???
			--evaluate (geomProgression 3.0 2.0 (-5)) `shouldBe` error "The value of n must be greater than 0 !"
			
        it "coprime" $ do
    	    coprime 10 15 `shouldBe` False
            coprime 12 35 `shouldBe` True		    
            coprime 10 (-15) `shouldBe` False
            coprime (-12) 35 `shouldBe` True
            coprime (-12) (-35) `shouldBe` True
            coprime 269 271 `shouldBe` True			
{- так как существует реализация данной функции, проверять буду с ней -}			
        it "gcd'" $ do
            gcd' 0 0 `shouldBe` gcd 0 0
            gcd' 1 1 `shouldBe` gcd 1 1
            gcd' 0 8 `shouldBe` gcd 0 8
            gcd' 3 0 `shouldBe` gcd 3 0
            gcd' 1 2 `shouldBe` gcd 1 2
            gcd' 10 100 `shouldBe` gcd 10 100
            gcd' 100 10 `shouldBe` gcd 10 100
            gcd' 101 400001 `shouldBe` gcd 101 400001
            gcd' 2424527370 75767567670 `shouldBe` gcd 2424527370 75767567670 			
            gcd' 269 271 `shouldBe` gcd 269 271 		
			
-- Пока не получилось отладить тесты для rbgToCmyk	
        it "rbgToCmyk" $ do
           rbgToCmyk RGB {red = 0,   green = 0,   blue = 0}   `shouldBe` CMYK {cyan = 0.0, magenta = 0.0, yellow = 0.0, black = 1.0}
           rbgToCmyk RGB {red = 255, green = 255, blue = 255} `shouldBe` CMYK {cyan = 0.0, magenta = 0.0, yellow = 0.0, black = 0.0}
           rbgToCmyk RGB {red = 10,  green = 10,  blue = 10}  `shouldBe` CMYK {cyan = 0.0, magenta = 0.0, yellow = 0.0, black = 0.9607843137254902}
           rbgToCmyk RGB {red = 100, green = 50,  blue = 25}  `shouldBe` CMYK {cyan = 0.0, magenta = 0.50, yellow = 0.75, black = 0.607843137254902}
           rbgToCmyk RGB {red = 180, green = 230, blue = 13}  `shouldBe` CMYK {cyan = 0.21739130434782605, magenta = 0.0, yellow = 0.9434782608695652, black = 9.80392156862745e-2} 

    describe "lists" $ do
	
        it "distance" pending
		    distance (Point []) (Point []) `shouldBe` 0
            distance (Point [0.0]) (Point [1.0]) `shouldBe` 1.0
            distance (Point [0.0, 0.0]) (Point [0.0, 1.0]) `shouldBe` 1.0
            distance (Point [1.0, 0.0]) (Point [0.0, 1.0]) `shouldBe` 2**(1/2)
            distance (Point [0.0, 0.0, 0.0]) (Point [1.0, 1.0, 1.0]) `shouldBe` 3**(1/2)
            distance (Point [1.0, 2.0, 3.0, 4.0]) (Point [5.0, 6.0, 7.0, 8.0]) `shouldBe` 8.0
            evaluate (distance (Point [0.0, 1.0, 0.0, 1.0, 0.1]) (Point [3.0, 4.0, 0.0])) `shouldThrow` anyException
            evaluate (distance (Point [0.0, 0.0, 0.0]) (Point [0.0, 1.0])) `shouldThrow` anyException
            evaluate (distance (Point [0.0, 0.0, 0.0]) (Point [0.0, 1.0, 1.0, 1.0])) `shouldThrow` anyException	
	
        it "findElement" pending
		    findElement 1 [] `shouldBe` False
		    findElement 1 [1, 2, 3, 4] `shouldBe` True
		    findElement 1 [4, 3, 2, 1] `shouldBe` True
		    findElement 5 [4, 3, 2, 1] `shouldBe` False
			
        it "intersect" pending
		    intersect [1, 2, 3, 4] [4, 5, 6, 7, 1] `shouldBe` [1, 4]
            intersect [1, 2, 3, 4] [5, 6, 7] `shouldBe` []
            intersect [] [] `shouldBe` []
            intersect [1, 1, 1, 1] [2, 2, 2, 2, 2] `shouldBe` []
            intersect [1, 2, 3, 4, 1] [1, 2, 3, 4, 1, 2] `shouldBe` [1, 2, 3, 4, 1]
			
		{- есть сложность с функцией zipN при работе, некорректная работа с -}
        it "zipN" pending
		
        it "ziphead" pending
		    ziphead [[1, 2, 3], [4, 5, 6], [7, 8, 9]] `shouldBe` [1,4,7]
			ziphead [[1, 2, 3], [4, 5], [6]] `shouldBe` [1,4,6]
		
		{- в ziptail возникает ошибка когда исходные списки разной длинны, в случаи последней обратоки-}
        it "ziptail" pending
		    ziptail [[1, 2, 3], [4, 5, 6], [7, 8, 9]] `shouldBe` [[2,3],[5,6],[8,9]]
			ziptail [[1, 2, 3], [4, 5], [6]] `shouldBe` [[2,3],[5],[]]
			
        it "find" pending
            find (> 0)  [-1, 2, -3, 4] 	`shouldBe` Just 2
            find (odd)  [-1, 1, -3, 4] 	`shouldBe` Just (-1)
            find (even) [-1, 1, -3, 5]  `shouldBe` Nothing
            find (> 0)  [-1, -2, -3]   	`shouldBe` Nothing
			
        it "findLast" pending
		    findLast (> 0) [-1, 2, -3, 4] `shouldBe` Just 4
            findLast (< 0) [-1, 2, -3, 4] `shouldBe` Just (-3)
            findLast (odd) [-1, 1, -3, 4] `shouldBe` Just (-3)
            findLast (even) [-1, 1, -3, 5] `shouldBe` Nothing
			
        it "mapFuncs" pending
            mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 `shouldBe` [9, 4, 0]
            mapFuncs [\x -> sqrt (-x), abs] (-4) `shouldBe` [2.0,4.0]
            mapFuncs'[\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 `shouldBe` [9, 4, 0]
            mapFuncs' [\x -> sqrt (-x), abs] (-4) `shouldBe` [2.0,4.0]
			
        it "satisfiesAll" $ do
            satisfiesAll [even, \x -> x `rem` 5 == 0] 10 `shouldBe` True
            satisfiesAll [] 4 `shouldBe` True
			
        it "tailNel" pending
            tailNel (NEL 1 [2,3]) `shouldBe` Just (NEL 2 [3])
            tailNel (NEL 1 [2]) `shouldBe` Just (NEL 2 [])
            tailNel (NEL 1 []) `shouldBe` Nothing
			
        it "lastNel" pending
            lastNel (NEL 1 [2,3]) `shouldBe` 3
            lastNel (NEL 1 []) `shouldBe` 1
            lastNel (NEL 1 [2]) `shouldBe` 2
		
        it "zipNel" pending
		    zipNel (NEL 1 [2,3]) (NEL 1 [2,3]) `shouldBe` Just (NEL (1,1) [(2,2),(3,3)])
            zipNel (NEL 1 []) (NEL 1 []) `shouldBe` Just (NEL (1,1) [])
            zipNel (NEL 1 [2]) (NEL 1 [3]) `shouldBe` Just (NEL (1,1) [(2,3)]) 
			
        it "listToNel" pending
            listToNel [1,2,3] `shouldBe` Just (NEL 1 [2,3])
            listToNel [1] `shouldBe` Just (NEL 1 [])
		
        it "nelToList" pending
            nelToList (NEL 1 [2,3]) `shouldBe` [1, 2, 3]
            nelToList (NEL 1 []) `shouldBe` [1]
		
    describe "luhn" $ it "" pending
            isLuhnValid 5536913875386613 `shouldBe` True
            isLuhnValid 4561261212345464 `shouldBe` False