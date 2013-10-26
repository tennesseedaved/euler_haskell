-- Euler project problem #7
-- generating primes
-- I've read there are several more efficient haskell algorithms generate primes

-- Shouldn't this have been problem number 1?

primes = sieve [2..]
	where sieve (p:xs) = p : sieve [x | x <- xs, mod x p > 0]
	
main = print $ primes !! 10000