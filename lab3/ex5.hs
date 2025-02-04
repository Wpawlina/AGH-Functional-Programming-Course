--zlozenie funkcji
import Data.List (sort)

f=(+1)
g=(*2)
h=(^3)

fg=f.g
gf=g.f
fgh=f.g.h

sortDesc :: Ord a => [a] -> [a]
sortDesc = reverse . sort










