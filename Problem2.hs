-- Euler problem #2
-- By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

-- Approach using fibonacci function to generate list
fib 0 = 1
fib 1 = 2
fib n = fib (n-1) + fib (n-2)

-- Defining list recursively. I didn't come up with this
fibs = 1 : 1 : [a+b | (a,b) <- zip fibs (tail fibs)]

-- Less efficient than using fibs because of so much recursion on fib, but I wanted to test
-- the direction I was going in the beginning
main = print (sum [x | x <- takeWhile (< 4000000) [y | y <- map fib [1..]], mod x 2 == 0])