module Renderer where

import LSystems
import IC.Colour
import IC.Graphics
import Examples -- to draw it in for REPL auto-import
import System.Random(StdGen)

drawLSystem1 :: Bool -> LSystem -> Int -> Colour -> IO ()
drawLSystem1 liveAction system n colour =
  drawLines liveAction (trace1 (expandLSystem system n) (angle system) colour)

drawLSystem2 :: Bool -> LSystem -> Int -> Colour -> IO ()
drawLSystem2 liveAction system n colour =
  drawLines liveAction (trace2 (expandLSystem system n) (angle system) colour)

{-|
  Additional draw function implementing the coloured trace function,
  which changes the colour of the line based on the depth of each command
-}
drawLSystem2col :: Bool -> LSystem -> Int -> Colour -> IO ()
drawLSystem2col liveAction system n colour =
  drawLines liveAction (trace2col (expandLSystem system n) (angle system) colour)

-- Probabilistic Drawers

-- Fresh random each call
drawProbLSystem2 :: Bool -> ProbLSystem -> Int -> Colour -> IO ()
drawProbLSystem2 liveAction psys n colour = do
  cmds <- expandProbLSystem psys n
  drawLines liveAction (trace2 cmds (angleP psys) colour)

-- Reproducible provided with the same StdGen each time
drawProbLSystem2WithSeed :: Bool -> ProbLSystem -> Int -> Colour -> StdGen -> IO ()
drawProbLSystem2WithSeed liveAction psys n colour g0 = do
  let (cmds, _g1) = expandProbLSystemWithSeed psys n g0
  drawLines liveAction (trace2 cmds (angleP psys) colour)

-- Draw the probabilistic system with colour changing:
drawProbLSystem2col :: Bool -> ProbLSystem -> Int -> Colour -> IO ()
drawProbLSystem2col liveAction psys n baseColour = do
  cmds <- expandProbLSystem psys n
  drawLines liveAction (trace2col cmds (angleP psys) baseColour)

onFrame :: Frame
onFrame = Frame
  {
  -- rotation forward in the screen's y axis
  fwd = (0, 1, 0)
  -- rotation to the right or left if negative
  ,right = (-1, 0, 1)
  -- rotation around +z into the screen,
  -- the bigger number helps to make
  -- some of the lines in the "shere"
  -- coincident whch looks better on screen.
  ,up = (0, 0, 3)
  }

drawSphere3D :: Bool -> IO ()
drawSphere3D live = do
  let sys     = mandala3D
      n       = 8
      -- bright cyan base
      colour  = Custom 0.3 0.8 1.0

      -- z start
      k       = 60
      -- projection plane
      k'      = 6

      -- custom values after testing
      gamma   = 0.3
      eps     = 1e-3
      
      ang     = angle3 sys
      cmds3d  = expandLSystem3 sys n

      start   = ((0,0,k), onFrame, colour)
      lines3D = trace3DFrom start cmds3d ang
      lines2D = projectLines3D eps k' gamma lines3D
  drawLines live lines2D