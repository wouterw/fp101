main :: IO ()
main = do
       putStrLn "What's your name?"
       name <- getLine
       -- let greeting = greet name
       putStrLn $ greet name

greet :: String -> String
greet name =
  "Hello, " ++ name ++ "!"
