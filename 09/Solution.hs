import Text.Parsec
import Data.List

simulate knots instructions = snd $ foldl run (take knots (repeat (0, 0)), []) instructions
    where run (state, acc) ins = (update state ins, (last $ update state ins):acc)

update (h@(hx, hy):t@(tx, ty):[]) i@(x, y) = [(hx + x, hy + y), (tx + (fst m), ty + (snd m))]
    where m = tailMovement h t i
update (h@(hx, hy):t@(tx, ty):s) i@(x, y) = (hx + x, hy + y):(update (t:s) (tailMovement h t i))

tailMovement (hx, hy) t@(tx, ty) (x, y) | legalTail t (hx + x, hy + y) = (0, 0)
                                        | otherwise = (step (hx + x) tx, step (hy + y) ty)
    where legalTail (tx, ty) (hx, hy) = (max (abs $ tx - hx) (abs $ ty - hy)) <= 1
          step a b = if a > b then (min 1 (a - b)) else (max (-1) (a - b))

parser = concat <$> (many1 line)
    where line = steps <$> ((oneOf "RULD") <* space) <*> (read <$> (many digit) <* newline)
          steps d n = take n $ ((repeat . match) d)
          match d = case d of 'R' -> (1, 0) ; 'U' -> (0, 1) ; 'L' -> (-1, 0) ; 'D' -> (0, -1)

solve = (\(Right x) -> (part1 x, part2 x)) <$> ((parse parser "") <$> readFile "input.txt")
    where part1 = length . nub . (simulate 2)
          part2 = length . nub . (simulate 10)
