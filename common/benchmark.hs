import Criterion.Main;

-- Your functions to benchmark
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

fastFib :: Int -> Int
fastFib n = fibHelper n 0 1
  where
    fibHelper 0 a b = a
    fibHelper n a b = fibHelper (n-1) b (a+b)

-- Benchmark suite
main :: IO ()
main = defaultMain [
  bgroup "fibonacci" [ bench "slow fib 20" $ whnf fibonacci 20
                     , bench "fast fib 20" $ whnf fastFib 20
                     ]
  ]