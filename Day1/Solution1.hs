myRead :: String -> Int
myRead (h:t) = if (h == '-') then (negate n) else n where
  n = read t :: Int
  
getInput :: IO [Int]
getInput = do
  content <- readFile "input.txt"
  let inputs = lines content
  let numInputs = map myRead inputs 
  return numInputs

solve1a :: Int -> [Int] -> Int
solve1a = foldl (+)

solution1a = do
  input <- getInput
  let solution = solve1a 0 input
  putStrLn (show solution)

partialSums :: [Int] -> [Int]
partialSums [] = []
partialSums xs = pSum 0 xs where
  pSum acc (x:xs) = (acc + x) : pSum (acc + x) xs
  pSum _ [] = []

getFirstRepeat :: Eq a => [a] -> Maybe a
getFirstRepeat [] = error "Empty List"
getFirstRepeat xs = go xs [] where
  go [] _ = Nothing
  go (x:xs) visited = if x `elem` visited
                         then Just x
                         else go xs (x:visited)


solve1b :: [Int] -> Maybe Int
solve1b input = getFirstRepeat $ partialSums (take 1000000 (cycle input))  --TODO FIX THIS IS TERRIBLE
  
solution1b = do
  input <- getInput
  putStrLn $ show $ solve1b input
  

