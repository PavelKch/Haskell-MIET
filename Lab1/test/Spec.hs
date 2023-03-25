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
        it "intersect" pending
        it "zipN" pending
        it "find" pending
        it "findLast" pending
        it "mapFuncs" pending
        it "tailNel" pending
        it "lastNel" pending
        it "zipNel" pending
        it "listToNel" pending
        it "nelToList" pending
    describe "luhn" $ it "" pending
