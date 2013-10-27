-- Euler project problem #9
-- A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
-- a2 + b2 = c2
-- For example, 32 + 42 = 9 + 16 = 25 = 52.
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

-- To reduce iteration of c, use replacement
-- c = 1000 - a - b => a^2 + b^2 = (1000 - (a + b))^2
-- a^2 + b^2 = 1000000  -2000(a+b) + a^2 + 2ab + b^2 => 0 = 1000000 - 2000a + 2ab - 2000b
-- => (-ab + 1000a + 1000b) = 500000
main = print $ [x*y*z | 
	(x,y) <- [(a,b) | a <- [1..999], b <- [1..999], a < b, (-(a*b) + (1000 * a) + (1000 * b)) == 500000], 
	let z = 1000 - x - y]