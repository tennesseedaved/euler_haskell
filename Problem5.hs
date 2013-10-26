-- Euler problem #5
-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

-- Probably much more verbose then it needs to be, but I am beginning to understand the language structure
-- a little better which should help cut down on the number of function declarations
-- Also, I definitely cut corners with hardcoding limits of 20 on some functions

-- generating primes
primes = sieve [2..]
	where sieve (p:xs) = p : sieve [x | x <- xs, mod x p > 0]
	
-- reducing primes list from infinite
limited_primes x = takeWhile (< x) primes

-- calculating all factors for number... kind of poor factors are passed as parameter
factor_list n [] = n : []
factor_list n (x:xs) 
	| n == 0 = []
	| mod n x == 0 = x : factor_list (div n x) (x:xs)
	| otherwise = factor_list n xs 
	
-- sets value in list at index
set_list n v xs = 
	let (l, r) = splitAt n xs
	in l ++ [v] ++ tail r
	
-- performs count sort on list
count_sort [] = replicate 20 0
count_sort (x:xs) = 
	let table = count_sort xs
	in set_list x (table !! x + 1) table 
	
-- list of factor lists in problem range
ranged_factors = [factor_list x (limited_primes 20) | x <- [1..20]]

-- zips together  list of lists with maximum value
find_max [] = replicate 20 0
find_max (x:xs) = [max a b | (a,b) <- zip x $ find_max xs]

-- finds product of sorted count list
product_count_list [] = 1
product_count_list xs =
	let c = last xs
	    v = length xs - 1
	in case c of 0 -> product_count_list $ init xs
	             n -> v * (product_count_list $ set_list (v) (n - 1) xs)
					
main = print (product_count_list $ find_max [count_sort x | x <- ranged_factors])