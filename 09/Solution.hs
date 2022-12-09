import Text.Parsec
import Data.List

runInstruction ((t, h), acc) ins = ((fst x, snd x), (fst x):acc)
    where x = newPos t h ins

newPos t@(tx, ty) (hx, hy) (x, y) | legalTail t newHead = ((tx, ty), newHead)
                                  | otherwise = ((tx + stepX, ty + stepY), newHead)
    where newHead = (hx + x, hy + y)
          legalTail (tx, ty) (hx, hy) = (max (abs $ tx - hx) (abs $ ty - hy)) <= 1
          stepX = step (fst newHead) tx
          stepY = step (snd newHead) ty
          step a b = if a > b then (min 1 (a - b)) else (max (-1) (a - b))

parser = concat <$> (many1 line)
    where line = steps <$> ((oneOf "RULD") <* space) <*> (read <$> (many digit) <* newline)
          steps d n = take n $ ((repeat . match) d)
          match d = case d of 'R' -> (1, 0) ; 'U' -> (0, 1) ; 'L' -> (-1, 0) ; 'D' -> (0, -1)

solve = (\(Right x) -> (part1 x)) <$> ((parse parser "") <$> readFile "input.txt")
    where part1 x = (length . nub . snd) $ foldl runInstruction (((0, 0), (0, 0)), []) x
