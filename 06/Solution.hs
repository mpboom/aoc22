import Data.List

findUnique n i s | length (nub (take n s)) == n = i + n
                 | otherwise = (findUnique n (i + 1) (drop 1 s))

solve = (\x -> (part1 x, part2 x)) <$> readFile "input.txt"
    where part1 = findUnique 4 0
          part2 = findUnique 14 0
