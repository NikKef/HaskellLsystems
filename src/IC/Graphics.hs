{-# LANGUAGE BangPatterns, TypeApplications, CPP #-}
module IC.Graphics (drawLines) where

import IC.Colour

import Control.Monad
import Data.Char
import Data.List
import Graphics.UI.GLUT hiding (Vertex, Red, Blue, Green)

type Vertex = (Float, Float)
type ColouredLine = (Vertex, Vertex, Colour)

drawLines :: Bool -> [ColouredLine] -> IO ()
drawLines live !ls = do
  getArgsAndInitialize
  w <- createWindow "LSystems"

#ifndef darwin_HOST_OS
  actionOnWindowClose   $= ContinueExecution
#endif
  displayCallback       $= display live ls
  keyboardMouseCallback $= Just keyboardMouse
  reshapeCallback       $= Just reshape

  mainLoop

display :: Bool -> [ColouredLine] -> IO ()
display live ls = do
  clear [ColorBuffer]

  let verticesOf (from, to, _)     = [from, to]
      vertices                     = concatMap verticesOf ls
      ((minX, minY), (maxX, maxY)) = boundingBox vertices

  preservingMatrix $ do
    ortho (realToFrac minX) (realToFrac maxX)
          (realToFrac minY) (realToFrac maxY)
          0 1
    if live then
      forM_ ls $ \line -> do
        renderPrimitive Lines (lineVertices line)
        flush
    else
      renderPrimitive Lines $
        forM_ ls $ \line ->
          lineVertices line

  flush

lineVertices :: ColouredLine -> IO ()
lineVertices ((fromX, fromY), (toX, toY), c) = do
  color  $ toColor3 c
  vertex $ Vertex3 @GLfloat (realToFrac fromX) (realToFrac fromY) 0
  vertex $ Vertex3 @GLfloat (realToFrac toX) (realToFrac toY) 0

toColor3 :: Colour -> Color3 GLfloat
toColor3 Black   = Color3 0 0 0
toColor3 White   = Color3 1 1 1
toColor3 Red     = Color3 1 0 0
toColor3 Green   = Color3 0 1 0
toColor3 Blue    = Color3 0 0 1
toColor3 Cyan    = Color3 0 1 1
toColor3 Magenta = Color3 1 0 1
toColor3 Yellow  = Color3 1 1 0
toColor3 (Custom r g b) = Color3 r g b

keyboardMouse :: Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse (Char '\ESC')   Down _ _ = leaveMainLoop
keyboardMouse (MouseButton _) Down _ p = print p
keyboardMouse _               _    _ _ = return ()

reshape :: Size -> IO ()
reshape s = do
  viewport $= (Position 0 0, s)
  postRedisplay Nothing

boundingBox :: [Vertex] -> (Vertex, Vertex)
boundingBox [] = ((100, 100), (100, 100))
boundingBox vs = foldl' f ((infinity, infinity), (-infinity, -infinity)) vs
  where f ((minX, minY), (maxX, maxY)) (x, y) =
            ((min minX x, min minY y), (max maxX x, max maxY y))
        infinity = read "Infinity"
