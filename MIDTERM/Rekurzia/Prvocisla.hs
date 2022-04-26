-- MatejM
sqrti::Integer->Integer
sqrti n = floor (sqrt $ fromIntegral n)

prime::Integer->Bool
prime 1 = False
prime n = null [d |d<-[2..(sqrti n)], mod n d == 0]

zlozene::Integer->Bool
zlozene n = not (prime n) && n /= 1

allPrimes::[Integer]
allPrimes = filter prime [2..]

primes::Integer->[Integer]
primes m = take (fromInteger m) allPrimes

kolkoPomocne::Integer->(Integer, Integer) 
kolkoPomocne n | not (zlozene potencialneZlozene) = kolkoPomocne (n+1)
                | otherwise = (n, potencialneZlozene)
                where potencialneZlozene = product (primes n) + 1

kolko::(Integer, Integer)
kolko = kolkoPomocne 0

najmensiePrvocisloTake::Integer
najmensiePrvocisloTake = head (filter (\p-> zlozene (2^p-1)) allPrimes)
