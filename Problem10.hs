-- Euler problem #10
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.

-- Pulled from haskell wiki
-- Got to say, even if I knew the sieve method well
-- I don't understand haskell lists well enough to make this 
-- efficient
primes = 2 : sieve primes [3..]
	where 
		sieve (p:ps) xs = let (h,t) = span (< p*p) xs
			in h ++ sieve ps [x | x<-t, rem x p /= 0]

-- So haskell processing times are not as magical as I hoped
main = print $ sum (takeWhile (< 2000000) primes)