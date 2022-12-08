import Data.List
import Text.Parsec

getVisibilities = last . (take 5) . (iterate (reverse . transpose . rowVisibility))
    where rowVisibility = map $ snd . (foldl tree (-1, []))
          tree (m, acc) ((v, s), h) | h > m = (h, acc ++ [((True, (getScore h acc:s)), h)])
                                    | otherwise = (m, acc ++ [((v, (getScore h acc:s)), h)])

getScore height acc | lowerTrees == (length acc) = lowerTrees
                    | otherwise = lowerTrees + 1
    where lowerTrees = length $ takeWhile (\(_, x) -> (height > x)) (reverse acc)

parser = many1 $ (many1 $ (\d -> ((False, []), read [d])) <$> digit) <* newline

solve = (\(Right x) -> (part1 x, part2 x)) <$> ((parse parser "") <$> (readFile "input.txt"))
    where part1 = length . (filter (== True)) . (map (fst . fst)) . concat . getVisibilities
          part2 = last . sort . (map product) . (map (snd . fst)) . concat . getVisibilities
