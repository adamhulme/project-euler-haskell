import Criterion.Main;

-- sol1, loop over each number and check divisible by 3 or 5
divisible n x = x `mod` n == 0
divisible2 m n x = x `mod` m == 0 || x `mod` n == 0
isDivisibleAny :: [Int] -> Int -> Bool
isDivisibleAny nums x = any (\n -> x `mod` n == 0) nums

sol1 n = sum $ filter (isDivisibleAny [3, 5]) [1..n]

-- sol2 : use gap pattern 3,2,1,3,1,2,3
sol2 :: Int -> Int
sol2 n = sum $ takeWhile (<= n) $ scanl (+) 0 (cycle [3,2,1,3,1,2,3])

-- sol3: using sum of arithmetic series scaled by k (discovered on stack overflow https://stackoverflow.com/questions/36374670/sum-of-multiples-of-3-and-5-below-1000-in-haskell?utm_source=chatgpt.com)
sol3 :: Int -> Int
sol3 n = sumDivisibleBy 3 + sumDivisibleBy 5 - sumDivisibleBy 15
  where
    sumDivisibleBy k =
      let p = n `div` k
      in k * p * (p + 1) `div` 2

main :: IO ()
main = do
    putStrLn $ "sol1 (n=999): " ++ show (sol1 999)
    putStrLn $ "sol2 (n=999): " ++ show (sol2 999)
    putStrLn $ "sol3 (n=999): " ++ show (sol3 999)

    putStr ""

    defaultMain [
      bgroup "Benchmark: task 1" [
          bench "sol1 (loop and filter)" $ whnf sol1 999,
          bench "sol2 (gaps)"            $ whnf sol2 999,
          bench "sol3 (closed-form)"     $ whnf sol3 999
        ]
      ]
