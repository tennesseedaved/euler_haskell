-- Euler problem #4
-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91  99.
-- Find the largest palindrome made from the product of two 3-digit numbers.
-- minimum product = 10000
-- maximum product < 1000000

import Data.Maybe

primes = sieve [100..]
	where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

-- Definitely not most efficient, need to get better with list manipulation
is_palindrome [] (x:xs) = False
is_palindrome (x:xs) [] = False
is_palindrome [] [] = True
is_palindrome (f:fs) (r:rs) = (f == r) && (is_palindrome fs rs)

-- Could be cleaner but was wresting with more fundamental problems then to worry about let expressions
multiple n [] = Nothing
multiple n (x:xs) 
	| (n `mod` x == 0) && (length (show x) == 3) && (length (show (div n x)) == 3) = Just (x, div n x)
	| otherwise = multiple n xs

-- Again this feels like a waste of a function, probably case is more appropriate
has_multiple Nothing = False
has_multiple (Just a) = True

-- Again, needs let expression to avoid repeated function... I would work on this but I've run out of time in day
find_palindrome [] = (0,0)
find_palindrome (x:xs)
	| has_multiple (multiple x [100..999]) = fromJust (multiple x [100..999])
	| otherwise = find_palindrome xs

main = print (find_palindrome [x | x <- reverse [10000..1000000], is_palindrome (show x) (reverse (show x))])