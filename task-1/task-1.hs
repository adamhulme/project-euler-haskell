divisible n x = x `mod` n == 0

main :: IO ()
main = putStrLn (show (filter (\x -> divisible 3 x || divisible 5 x) [1..999]))