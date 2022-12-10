import Data.Char

updateResult (x, 2) = (x, x)
updateResult (1, 1) = (1, 3)
updateResult (2, 1) = (2, 1)
updateResult (3, 1) = (3, 2)
updateResult (1, 3) = (1, 2)
updateResult (2, 3) = (2, 3)
updateResult (3, 3) = (3, 1)

score d = sum (map scorePerMatch d)
    where scorePerMatch (a, b) | a == b = 3 + b
                               | b == (a `mod` 3) + 1 = 6 + b
                               | otherwise = b

parse raw = map (\x -> (ord (x !! 0) - 64, ord (x !! 2) - 87)) (lines raw)

solve = ((\x -> (part1 x, part2 x)) . parse) <$> readFile "input.txt"
    where part1 = score
          part2 = score . (map updateResult)
