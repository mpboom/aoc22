import Data.List

reader x (a, b) | x == "" = (a ++ [b], 0)
                | otherwise = (a, b + (read x))

parse raw = reverse $ (fst i) ++ [snd i]
    where i = foldr reader ([], 0) (lines raw)

solve = ((\x -> (part1 x, part2 x)) . parse) <$> readFile "input.txt"
    where part1 = head . reverse . sort
    where part2 = sum . (take 3) . reverse . sort
