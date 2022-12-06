import Data.List
import Text.Parsec

data Puzzle = Puzzle [[Char]] [(Int, Int, Int)]

runPuzzle q (Puzzle stacks instructions) = foldl (move q) stacks instructions
    where move q s i = map (move' q s i) (zip s [1..])
          move' q s (n, f, t) (stack, idx) | idx == f = drop n stack
                                           | idx == t = (q (take n (s !! (f - 1)))) ++ stack
                                           | otherwise = stack

parseGrid = (++) <$> (many (row a)) <*> (many (row b)) <* (many (digit <|> (char ' '))  <* newline)
    where row start = (:) <$> (start <* (char ' ')) <*> (sepBy (a <|> b) (char ' ')) <* newline
          a = between (char ' ') (char ' ') (char ' ')
          b = between (char '[') (char ']') letter

parseInstructions = many (instruction <* newline)
    where instruction = (,,) <$> (read <$> (string "move " *> (many digit)))
                             <*> (read <$> (string " from " *> (many digit)))
                             <*> (read <$> (string " to " *> (many digit)))

parser = strip <$> (Puzzle <$> (parseGrid <* newline) <*> parseInstructions)
    where strip (Puzzle s i) = Puzzle (map (\row -> [x | x <- row, x /= ' ']) (transpose s)) i

solve = ((\(Right x) -> (part1 x, part2 x)) . (parse parser "")) <$> readFile "input.txt"
    where part1 = (map head) . (runPuzzle reverse)
          part2 = (map head) . (runPuzzle id)
