import Data.List
import Text.Parsec

data Puzzle = Puzzle [[Char]] [(Int, Int, Int)] deriving Show

runPuzzle _ x@(Puzzle _ []) = x
runPuzzle q x = runPuzzle q (doInstruction x q)

doInstruction (Puzzle s ((n, f, t):is)) q = Puzzle (map move (zip s [1..])) is
    where move (stack, idx) | idx == f = drop n stack
                            | idx == t = (q (take n (s !! (f - 1)))) ++ stack
                            | otherwise = stack

getStacks (Puzzle s _) = s

parseGrid = (++) <$> (many (normalRow emptyCell)) <*> (many (normalRow normalCell)) <* finalRow
    where normalRow start = (:) <$> (start <* (char ' '))
                                <*> (sepBy (normalCell <|> emptyCell) (char ' '))
                                <* newline
          finalRow = (sepBy (between (char ' ') (char ' ') digit) (char ' ')) <* newline
          normalCell = between (char '[') (char ']') letter
          emptyCell = (\_ -> ' ') <$> count 3 (char ' ')

parseInstructions = many (instruction <* newline)
    where instruction = (,,) <$> (read <$> (string "move " *> (many digit)))
                             <*> (read <$> (string " from " *> (many digit)))
                             <*> (read <$> (string " to " *> (many digit)))

preparePuzzle (Puzzle s i) = Puzzle (map (\row -> [x | x <- row, x /= ' ']) (transpose s)) i

parser = preparePuzzle <$> (Puzzle <$> (parseGrid <* newline) <*> parseInstructions)

solve = ((\(Right x) -> (part1 x, part2 x)) . (parse parser "")) <$> readFile "input.txt"

part1 puzzle = map head (getStacks $ runPuzzle reverse puzzle)

part2 puzzle = map head (getStacks $ runPuzzle id puzzle)
