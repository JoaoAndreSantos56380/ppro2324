-- Importing QuickCheck
import Test.QuickCheck

-- A simple function to test: reversing a list twice should give the original list
doubleReverse :: [a] -> [a]
doubleReverse = reverse . reverse

-- Property: Reversing a list twice returns the original list
prop_DoubleReverse :: [Int] -> Bool
prop_DoubleReverse xs = doubleReverse xs == xs

-- Main function to run the QuickCheck test
main :: IO ()
main = quickCheck prop_DoubleReverse
