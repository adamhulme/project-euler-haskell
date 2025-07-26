divisible n x = x `mod` n == 0
divisible2 m n x = x `mod` m == 0 || x `mod` n == 0
isDivisibleAny :: [Int] -> Int -> Bool
isDivisibleAny nums x = any (\n -> x `mod` n == 0) nums

main :: IO ()
main = putStrLn (show (sum (filter (\x -> isDivisibleAny [3,5] x) [1..999])))