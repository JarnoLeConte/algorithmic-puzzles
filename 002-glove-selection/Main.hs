module Main where

black = 10
brown = 6
gray = 4

drawer :: [Int]
drawer = [black, brown, gray]

matchOne :: Int
matchOne = sum (map (`div` 2) drawer) + 1

matchAll :: Int
matchAll = sum drawer - (minimum drawer `div` 2) + 1

main :: IO ()
main = do
  putStrLn $ "Gloves { black = " ++ show black ++ ", brown = " ++ show brown ++ ", gray = " ++ show gray ++ " }"
  putStrLn $ "The smallest number of gloves you need to select to guarantee getting the following."
  putStrLn $ "(a) At least one matching pair: " ++ show matchOne
  putStrLn $ "(b) At least one matching pair of each color: " ++ show matchAll
