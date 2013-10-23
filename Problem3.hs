-- Euler problem #3
-- The prime factors of 13195 are 5, 7, 13 and 29. What is the largest prime factor of the number 600851475143 ?

-- I was looking for way to generate primes recursively, I don't know if mine would have worked
-- but I think my syntax was problematic, I came across this while trying to fix mine
primes = sieve [2..]
	where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

-- I don't really like this, because I felt like I should have a clearer expression of intent
-- and it feel weird passing a list of primes.
-- I'm glad to just use the guard style function definition though and it's more efficient than checking all primes
-- up to the number
reduce (p:xs) n
 	| n == p = p
	| n `mod` p == 0 = reduce (p:xs) (n `div` p)
	| otherwise = reduce xs n
	
main = print (reduce primes 600851475143)