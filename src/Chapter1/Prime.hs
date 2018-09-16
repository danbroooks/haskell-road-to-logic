module Chapter1.Prime
  ( divides
  , leastDivisor
  , leastDivisor'
  , isPrime
  , primes
  , sortList
  , count
  , blowup
  , strString
  , substring
  , lengths
  , sumLengths
  ) where

divides :: Integral a => a -> a -> Bool
divides d n = (n `rem` d) == 0 -- [1]

leastDivisor :: Integer -> Integer
leastDivisor = ldf 2
  where
    ldf k n | divides k n = k
            | k ^ 2 > n = n -- [0]
            | otherwise = ldf (k + 1) n

leastDivisor' :: Integer -> Integer
leastDivisor' = ldpf primes
  where
    ldpf (p:ps) n | n `rem` p == 0 = p
                  | p ^ 2 > n = n
                  | otherwise = ldpf ps n

isPrime :: Integer -> Either String Bool
isPrime n | n < 1     = Left "Not a positive integer"
          | n == 1    = Right False
          | otherwise = Right (leastDivisor n == n)

primes :: [Integer]
primes = filter isPrime' [2..]
  where
    isPrime' n =
      case isPrime n of
        Right prime -> prime
        Left _ -> False

listMin :: Ord a => [a] -> Maybe a
listMin = selectOrd min

listMax :: Ord a => [a] -> Maybe a
listMax = selectOrd max

selectOrd :: Ord a => (a -> a -> a) -> [a] -> Maybe a
selectOrd f = foldr pick Nothing
  where
    pick a =
      pure . maybe a (f a)

removeFst :: Eq a => a -> [a] -> [a]
removeFst a as =
  case as of
    [] -> []
    h : t ->
      if h == a then t
                else h : removeFst a t

sortList :: (Eq a, Ord a) => [a] -> [a]
sortList as = maybe [] doSort (listMin as)
  where
    doSort n =
      n : sortList (removeFst n as)

count :: Char -> String -> Int
count match = foldr ((+) . chValue) 0
  where
    chValue ch | ch == match = 1
               | otherwise   = 0

blowup :: String -> String
blowup = go 1
  where
    go _ "" = ""
    go n (h : t) = replicate n h ++ go (n + 1) t

strString :: [String] -> [String]
strString = sortList

prefix :: String -> String -> Bool
prefix [] _ = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys

substring :: String -> String -> Bool
substring xs ys | prefix xs ys = True
                | otherwise = check ys
  where
    check (_:ys') = substring xs ys'
    check _ = False

lengths :: [[a]] -> [Int]
lengths = fmap length

sumLengths :: [[a]] -> Int
sumLengths = foldr ((+) . length) 0

{- [0] Exercise 1.4:
         Any k that satisfies `k ^ 2 == n` would also
         satisfy `divides k n` and thus would have
         already returned `k`

   [1] Exercise 1.6:
         rem :: Integral a => a -> a -> a
-}
