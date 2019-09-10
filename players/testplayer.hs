import System.IO

main = gameLoop

gameLoop = do
    getLine
--    appendFile "obaplayertest.log" (input ++ "\n")
    putStrLn "4 0 6 0 5 1 5 6"
    hFlush stdout
    gameLoop
