import Text.Parsec

atCycle cycles cycle = 1 + (sum $ take (cycle - 1) cycles)

draw cycles = reverse $ foldl f "" (zip [1..(length cycles)] (concat (repeat [0..39])))
    where f acc (n, i) | (atCycle cycles n) `elem` [i - 1, i, i + 1] = '\9608':acc
                       | otherwise = ' ':acc

parser = concat <$> (many1 (noop <|> add))
    where noop = (\_ -> [0]) <$> (string "noop") <* newline
          add = (string "addx ") *> (negative <|> ((\x -> [0, read x]) <$> (many1 digit))) <* newline
          negative = (\x -> [0, (*) (-1) (read x)]) <$> (char ('-') *> (many1 digit))

solve = do
    (Right x) <- (parse parser "") <$> readFile "input.txt"
    points <- return [0, 40, 80, 120, 160, 200]
    putStrLn $ show $ sum [(atCycle x (y + 20)) * (y + 20) | y <- points]
    putStrLn $ (concat . (map ((++ "\n")))) [(take 40) . (drop y) $ draw x | y <- points]
