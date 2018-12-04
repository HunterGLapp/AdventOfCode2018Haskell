myRead :: String -> Int
myRead (h:t) = if (h == '-') then (negate n) else n where
  n = read t :: Int
  
getInput :: IO [Int]
getInput = do
  content <- readFile "input.txt"
  let inputs = lines content
  let numInputs = map myRead inputs 
  return numInputs

solve :: Int -> [Int] -> Int
solve = foldl (+)

main = do
  input <- getInput
  let solution = solve 0 input
  putStrLn (show solution)
  
