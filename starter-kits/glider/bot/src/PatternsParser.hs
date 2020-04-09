module PatternsParser ( readShapes
                      , readInstructions
                      , getShape
                      , getInstruction
                      )
  where

import qualified Entities as E

import Data.Either
import Data.Maybe
import Data.List
import Text.Parsec
import Text.Parsec.Char (char)



patternParser :: Parsec String () (String, [([(Int, Int)], [(Int, Int)])])
patternParser = (fmap (\x y -> (x, y)) labelParser) <*
                (string "----\n") <*>
                endBy shapeParser (string "----\n")

labelParser :: Parsec String () String
labelParser = (string "#") *> spaces *> many (noneOf " \n") <* (skipMany (char ' ')) <* char '\n'

shapeParser :: Parsec String () ([(Int ,Int)], [(Int, Int)])
shapeParser = fmap shapeInterpreter (lineParser `sepBy` (char '\n'))
  where
    shapeInterpreter :: [[(Int, Bool)]] -> ([(Int, Int)], [(Int, Int)])
    shapeInterpreter lst = foldl pros2 ([], []) $ snd $ foldl pros1 (0, [])lst
      where
        pros1 (c, res) l = (c+1, res++(map (format c) l))
        pros2 (shp, cmds) (x, y, iscmd) = if iscmd
                                          then (shp, (x, y):cmds)
                                          else ((x, y):shp, cmds)

        format x (y, typ) = (x, y, typ)

    lineParser :: Parsec String () [(Int, Bool)]
    lineParser = fmap lineInterpreter $ many $ oneOf "X* "

    lineInterpreter :: String -> [(Int, Bool)]
    lineInterpreter str = snd $ foldl pros (0, []) str
      where
        pros (c, res) s = case s of
            'X' -> (c+1, (c,False):res)
            '*' -> (c+1, (c,True):res)
            ' ' -> (c+1, res)


readShapes :: IO [(String, E.Shape)]
readShapes = do
  raw <- readFile "patterns/shapes.txt"
  let parsed = parse (many patternParser) "" raw
  let formated = map (\(name, (coords, _):_) -> (name, E.S coords)) $ fromRight [] parsed
  return formated

readInstructions :: IO [(String, [E.Instruction])]
readInstructions = do
  raw <- readFile "patterns/instructions.txt"
  let parsed = parse (many patternParser) "" raw
  let formated = map (\(name, lst) -> (name, map (\(shp, instr) -> E.Instr (E.S shp) instr) lst)) $ fromRight [] parsed
  return formated

getShape :: [(String, E.Shape)] -> String -> E.Shape
getShape shps name = snd $ fromJust $ find (\(n, _) -> n == name) shps

getInstruction :: [(String, [E.Instruction])] -> String -> [E.Instruction]
getInstruction insts name = snd $ fromJust $ find (\(n, _) -> n == name) insts
