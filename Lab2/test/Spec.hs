import Poly
import SimpleLang
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "poly" $ do
        it "applyPoly" $ pending
            applyPoly (P [1]) 0 `shouldBeв` 1  
            applyPoly (P [1]) 1 `shouldBeв` 1  
            applyPoly (P [1]) 100 `shouldBeв` 1  
            applyPoly (P [0, 1]) 1 `shouldBeв` 1  
            applyPoly (P [0, 1]) 0 `shouldBeв` 0  
            applyPoly (P [0, (-1)]) 1 `shouldBeв` (-1)  
            applyPoly (P [0, (-1)]) 0 `shouldBeв` 0  
            applyPoly (P [0, 0, 1]) 0 `shouldBeв` 0  
            applyPoly (P [0, 0, 1]) 1 `shouldBeв` 1  
            applyPoly (P [0, 0, 1]) (-1) `shouldBeв` 1  
            applyPoly (P [1, 1, 1, 1, 1, 1, 1]) 10 `shouldBe` ((10^0)  + (10^1) + (10^2) + (10^3) + (10^4) + (10^5) + (10^6))
        it "(==)" $ do
            (P [1]) == (P [1, 0, 0, 0, 0]) `shouldBe` True
            (P [1, 1, 1, 1]) == (P [1, 1, 1, 1, 0]) `shouldBe` True
            (P [1, 2, 3, 4]) == (P [1, 2, 3, 4]) `shouldBe` True
            (P [(-1), (-2), (-3), (-4)]) == (P [(-1), (-2), (-3), (-4)]) `shouldBe` True
            (P [(-1), (-2), (-3), 4]) == (P [(-1), (-2), (-3), 4]) `shouldBe` False
        it "show" $ do
            show (P [1]) `shouldBe` "1"
            show (P [1, 0]) `shouldBe` "1"
            show (P [1, 0, 0, 0, 0, 0, 0, 0, 0, 0]) `shouldBe` "1"
            show (P [1, 2, 3, 4, 5]) `shouldBe` "5x^4 + 4x^3 + 3x^2 + 2x + 1"	
            show (P [123, 456, 789, 987, 654, 321]) `shouldBe` "321x^5 + 654x^4 + 987x^3 + 789x^2 + 456x + 123"
        it "plus" $ do
            plus (P [1]) (P [1]) `shouldBe` (P [2])
            plus (P [1, 2, 3, 4, 5, 6]) (P [6, 5, 4, 3, 2, 1]) `shouldBe` (P [7, 7 , 7, 7, 7, 7])
            plus (P [1, 0, 0, 0, 0, 0, 1]) (P [0, 1, 1, 1, 1, 1, 0]) `shouldBe` (P [1, 1, 1, 1, 1, 1, 1])
        it "times" $ do
            times (P [1]) (P [1]) `shouldBe` (P [1])
            times (P [1, 1]) (P [1, 1]) `shouldBe` (P [1, 2, 1])
            times (P [1, 2, 3, 4]) (P [4, 3, 2, 1]) `shouldBe` (P [4, 11, 20, 30, 20, 11, 4])
			
    describe "simpleLang" $ do
        -- включите тесты на работу 
        it "desugar" $ pending
