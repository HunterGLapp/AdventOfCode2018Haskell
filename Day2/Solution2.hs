getInput :: IO [String]
getInput = do
  content <- readFile "input.txt"
  return (lines content)


