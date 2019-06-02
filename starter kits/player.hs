import System.IO

main = do
  getLine
  putStrLn "Lugarun"
  hFlush stdout
  turn

turn = do
  putStrLn "1 2 1 3 1 4 1 5 1 6"    
  getLine
  putStrLn "1 3 1 4 1 5 1 6"    
  getLine
  putStrLn "1 3 1 4"    
  getLine
  empty
  turn

empty = do
  putStrLn ""
  getLine
  empty
