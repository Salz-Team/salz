import System.IO
import Data.List

main = do
    makeGlider
    end

--  X
-- X
-- XXX

-- X
-- XXX
--  X

makeGlider = do
    raw <- getLine
    let x = (map read $ words raw)!!0
    let y = (map read $ words raw)!!1
    let commands = [x, y-1, x-1, y, x-1, y+1, x, y+1, x+1, y+1, x, y]
    putStrLn $ intercalate " " $ map show commands




end = do
    getLine
    putStrLn ""
    end

