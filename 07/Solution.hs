import Text.Parsec
import Data.List

data Directory = Directory [Int] [Directory]

parser = Directory <$> (cd *> ls) <*> (many $ try parser) <* (optional $ string "$ cd ..\n")
    where cd = (string "$ cd ") >> (many $ letter <|> (char '/')) >> string "\n$ ls\n"
          ls = many1 $ ((\_ -> 0) <$> ((string "dir ") >> (many letter) >> newline)) <|> dir
          dir = (read <$> (many digit)) <* (char ' ' >> (many $ letter <|> (char '.')) >> newline)

dirsize d@(Directory s ds) = (sum $ files d):(concat $ map dirsize ds)
    where files (Directory s ds) = s ++ (concat (map files ds))

solve = (\(Right x) -> (part1 x, part2 x)) <$> ((parse parser "") <$> readFile "input.txt")
    where part1 = sum . (filter (<= 100000)) . dirsize
          part2 x = head . sort . (filter (>= (-) (head . dirsize $ x) 40000000)) $ dirsize x
