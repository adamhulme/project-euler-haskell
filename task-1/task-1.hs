divisible n x = x `mod` n == 0
divisible2 m n x = x `mod` m == 0 || x `mod` n == 0

main :: IO ()
main = putStrLn (show (sum (filter (\x -> divisible2 3 5 x) [1..999])))