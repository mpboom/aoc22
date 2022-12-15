import Data.List
import Data.Maybe
import Text.Parsec

data List = List [List] | Val Int deriving Eq

markers = [(List [(List [(Val 2)])]), (List [(List [(Val 6)])])]

inOrder (a, b) = fromJust $ inOrder' a b

inOrder' l@(Val a) r@(List b) = inOrder' (List [l]) r
inOrder' l@(List a) r@(Val b) = inOrder' l (List [r])
inOrder' (Val l) (Val r) | l == r = Nothing
                         | otherwise = Just (l < r)
inOrder' (List []) (List []) = Nothing
inOrder' a@(List []) b@(List (r:rs)) = Just True
inOrder' a@(List (l:ls)) b@(List []) = Just False
inOrder' a@(List (l:ls)) b@(List (r:rs)) | isJust current = current
                                               | otherwise = next
    where current = inOrder' l r
          next = inOrder' (List ls) (List rs)

markerIndices lists = f $ sortBy g (concat $ (map (\(l, r) -> [l, r]) lists) ++  [markers])
    where f list = findIndices (== True) $ map ((flip elem) markers) list
          g a b = if (inOrder (a, b)) then LT else GT

parser = many1 (pair <* (((\_ -> ()) <$> newline) <|> eof))
    where pair = (,) <$> (list <* newline) <*> (list <* newline)
          list = (List <$> between (char '[') (char ']') (sepBy list (char ','))) <|> (Val <$> (read <$> (many1 digit)))

solve = (\(Right x) -> (part1 x, part2 x)) <$> ((parse parser "") <$> readFile "input.txt")
    where part1 x = sum $ map (\(a, b) -> if a then b else 0) (zip (map inOrder x) [1..])
          part2 x = (product . (map (+ 1))) $ (markerIndices x)
