module Main where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)

import Effect (Effect)
import Effect.Console (log)
import Graphics.Canvas (getContext2D
                       ,fillPath
                       ,getCanvasElementById
                       ,Context2D
                       ,Rectangle
                       ,setFillStyle
                       ,rect)
import Signal.DOM (mousePos)
import Signal (foldp, Signal, runSignal)



-- Model
type Point = Tuple Int Int

type Model = 
  { xd :: Int
  , yd :: Int
  , size :: Int
  , mouse :: Point
  }


start :: Model
start =
  { xd : 400
  , yd : 400
  , size : 10
  , mouse : Tuple 20 20
  }

-- Update
type Input = Point

set :: Point -> Model -> Model
set (Tuple x y) m = m { mouse = Tuple (mod x m.xd) (mod y m.yd) }

-- View
render :: Model -> Effect Unit
render m = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  --walls
  setFillStyle ctx wallColor
  fillPath ctx $ rect ctx
    { x: 0.0
    , y: 0.0
    , width: toNumber $ m.xd
    , height: toNumber $ m.yd
    }
  -- mouse
  colorSquare m.size (m.mouse) mouseColor ctx


colorSquare :: Int -> Point -> String -> Context2D -> Effect Unit
colorSquare size (Tuple x y) color ctx = do
  setFillStyle ctx color
  fillPath ctx $ rect ctx $ square size x y

square :: Int -> Int -> Int -> Rectangle
square  size x y =
  { x: toNumber $ x
  , y: toNumber $ y
  , width: toNumber $ size
  , height: toNumber $ size
  }

wallColor = "#000000"
mouseColor = "#FFFFFF"

-- Signal
input :: Effect (Signal Input)
input = map (map (\x -> Tuple x.x x.y)) mousePos

main :: Effect Unit
main = do
  msignal <- input
  render start
  let game = foldp set start msignal
  runSignal (map render game)
