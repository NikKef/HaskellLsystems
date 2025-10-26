-- Use the following command to generate the submitted image:
-- ghci> drawSphere3D False

module LSystems ( LSystem(LSystem), ColouredLine, Command(..)
                , angle, axiom, rules, lookupChar
                , expandOne, expand, move, parse, trace1, trace2, trace3, trace2col
                , expandLSystem
                , ProbLSystem(ProbLSystem)
                , angleP, axiomP, rulesP, lookupP
                , expandOneP, expandP
                , expandProbLSystem, expandProbLSystemWithSeed
                , LSystem3(LSystem3), Command3(..), Frame(..)
                , angle3, axiom3, rules3, trace3DFrom
                , expandLSystem3, projectLines3D) where

import IC.Colour

-- For the probabilistic LSystem implementation
import System.Random (randomIO, mkStdGen, StdGen, random)

import Data.Maybe (mapMaybe)

-- Part 1
----------------------------------------------------------
type Rules a = [(Char, [a])]
data LSystem = LSystem Float [Char] (Rules Char)

-- Returns the rotation angle for the given LSystem.
angle :: LSystem -> Float
angle (LSystem theta _ _) = theta

-- Returns the axiom string for the given LSystem.
axiom :: LSystem -> [Char]
axiom (LSystem _ ax _) = ax

-- Returns the set of rules for the given LSystem.
rules :: LSystem -> Rules Char
rules (LSystem _ _ rls) = rls

{-|
  Arguments:
    > Rules of type a
    > A character search value
  Returns:
    > the binding of the search value in the ruleset given
  Pre: the character has a binding in the Rules list

  Since the binding exists, we can pattern match on the Just constructor
  of the result of the prelude lookup function.
-}
lookupChar :: Rules a -> Char -> [a]
lookupChar rs searchVal = justVal
  where
    (Just justVal) = lookup searchVal rs

-- Part 2: Expansion and Parsing
data Command = F | L | R | B [Command] deriving Show

{-|
  Arguments:
    > Rule table of Characters r
    > Sting s
  Returns:
    > Expanded string s, once using the rules of table r
-}
expandOne :: Rules Char -> [Char] -> [Char]
expandOne = concatMap . lookupChar

{-|
  Arguments:
    > Rule table of Characters r
    > String s
    > Number of times to expand the string
  Returns:
    > Expanded string n times
-}
expand :: Rules Char -> [Char] -> Int -> [Char]
expand r s n = iterate (expandOne r ) s !! n

{-|
  Arguments:
    > String to be parsed
  Returns:
    > List of Commands by parsing each character of the input string
-}
parse :: [Char] -> [Command]
parse = fst . parseCommands

{-|
  Helper function used for the parsing of a string
  This is a Parser of commands that saves for each
  recursive call the current list of commands parsed
  and the rest of the string to be parsed.
-}
parseCommands :: String -> ([Command], String)
parseCommands [] = ([], [])
parseCommands (']':cs) = ([], cs)
parseCommands ('[':cs) = (bracketedCmd : laterCmds, rest2)
  where
    (nestedCmds, rest1) = parseCommands cs
    bracketedCmd = B nestedCmds
    (laterCmds, rest2) = parseCommands rest1
parseCommands (c:cs) = (thisCmdList ++ laterCmds, rest)
  where
    thisCmdList = lookupChar commandMap c
    (laterCmds, rest) = parseCommands cs

{-|
  Arguments:
    > LSystem to be expanded
    > Nmuber of times to expand the LSystem
  Returns:
    > The list of parsed commands when expanding the
      LSystem n times
-}
expandLSystem :: LSystem -> Int -> [Command]
expandLSystem ls = parse . expand (rules ls) (axiom ls)

-- Part 3: Turtles
type Vertex = (Float, Float)
type TurtleState = (Vertex, Float)
type ColouredLine = (Vertex, Vertex, Colour)

-- Constant containing the initial position and direction of the turle
-- Position is the origin, with the direction parallel to the y-axis
initialState :: TurtleState
initialState = ((0, 0), 90)

{-|
  Move a turtle.
  F moves distance 1 in the current direction.
  L rotates left according to the given angle.
  R rotates right according to the given angle.

  Arguments:
    > The command to be executed
    > The LSystem creation angle
    > The current state of the turtle
  Returns:
    > The modified state of the turtle depending
      on the command inputted
-}
move :: Command -> Float -> TurtleState -> TurtleState
move L a (v, va) = (v, va + a)
move R a (v, va) = (v, va - a)
move F a ((vx, vy), va) = ((vx + cos toRad, vy + sin toRad), va)
  where
    toRad = degreesToRadians va

{-|
  Trace funtion that uses recursion to create the list of Coloured
  Lines to be created.
-}
trace1 :: [Command] -> Float -> Colour -> [ColouredLine]
trace1 cs t col = trace1' cs initialState
  where
    trace1' :: [Command] -> TurtleState -> [ColouredLine]

    trace1' [] _ = []

    trace1' (B cmds : cmds') st =
      trace1' cmds st ++ trace1' cmds' st

    trace1' (F : cmds') st@(vxy, _) =
      let
        newS@(newV, _) = move F t st
      in
        (vxy, newV, col) : trace1' cmds' newS

    trace1' (cmd : cmds') st = trace1' cmds' (move cmd t st)

{-|
  This version uses an explicit stack of residual commands and turtle states to
  construct the Coloured Line list.
-}
trace2 :: [Command] -> Float -> Colour -> [ColouredLine]
trace2 cs t col = trace2' cs initialState []
  where
    trace2' :: [Command] -> TurtleState -> [([Command], TurtleState)] -> [ColouredLine]

    trace2' [] _ [] = []

    trace2' [] _ ((cmds, thisS):cmdStack) = trace2' cmds thisS cmdStack

    trace2' (B cmds : cmds') currentS cmdStack =
      trace2' cmds currentS ((cmds', currentS):cmdStack)

    trace2' (F : cmds') currentS@(vxy, _) cmdStack =
      let
        newS@(newV, _) = move F t currentS
      in
        (vxy, newV, col) : trace2' cmds' newS cmdStack

    trace2' (cmd : cmds') currentS cmdStack =
      trace2' cmds' (move cmd t currentS) cmdStack

{-|
  These versions of trace2 and move change the colour of the line each time 
  the turtle goes deeper into the structure (inside a bracketed command).
  Part of the Further Extensions to the LSystem. This includes the addition
  of drawLSystem2col to draw the shapes using these functions and nextColour
  to change one colour to another based on its position in the colour wheel.
-}
type TurtleStateCol = (Vertex, Float, Colour)

moveCol :: Command -> Float -> TurtleStateCol -> TurtleStateCol
moveCol L a (v, va, c) = (v, va + a, c)
moveCol R a (v, va, c) = (v, va - a, c)
moveCol F a ((vx, vy), va, c) = ((vx + cos toRad, vy + sin toRad), va, c)
  where
    toRad = degreesToRadians va

trace2col :: [Command] -> Float -> Colour -> [ColouredLine]
trace2col cs t col = trace2col' cs initialStateCol []
  where
    initialStateCol :: TurtleStateCol
    initialStateCol = ((0, 0), 90, col)

    trace2col' :: [Command] -> TurtleStateCol -> [([Command], TurtleStateCol)] -> [ColouredLine]

    trace2col' [] _ [] = []

    trace2col' [] _ ((cmds, thisS):cmdStack) = trace2col' cmds thisS cmdStack

    trace2col' (B cmds : cmds') currentS@(v, va, c) cmdStack =
      trace2col' cmds (v, va, nextColour c) ((cmds', currentS):cmdStack)

    trace2col' (F : cmds') currentS@(vxy, _, statecol) cmdStack =
      let
        newS@(newV, _, thisCol) = moveCol F t currentS
      in
        (vxy, newV, thisCol) : trace2col' cmds' newS cmdStack

    trace2col' (cmd : cmds') currentS cmdStack =
      trace2col' cmds' (moveCol cmd t currentS) cmdStack

{-|
  This version uses an accumulating parameter, making the function
  tail recursive. After some benchmarking, I found out that this
  version is much slower than the trace1 and trace2 functions. This
  may be because the use of reverse increases the time complexity.
-}
trace3 :: [Command] -> Float -> Colour -> [ColouredLine]
trace3 cs t col = reverse (trace3' cs initialState [] [])
  where
    trace3' :: [Command] -> TurtleState -> [([Command], TurtleState)] -> [ColouredLine] -> [ColouredLine]

    trace3' [] _ [] cl = cl

    trace3'[] _ ((cmds, thisS):cmdStack) cl =
      trace3' cmds thisS cmdStack cl

    trace3'(B cmds : cmds') currentS cmdStack cl =
      trace3' cmds currentS ((cmds', currentS):cmdStack) cl

    trace3' (F : cmds') currentS@(vxy, _) cmdStack cl =
      let
        newS@(newV, _) = move F t currentS
      in
        trace3' cmds' newS cmdStack ((vxy, newV, col) : cl)

    trace3' (cmd : cmds') currentS cmdStack cl =
      trace3' cmds' (move cmd t currentS) cmdStack cl

-- Further Extensions

-- Probabilistic LSystem:
------------------------------------------------------------------------------

-- This section includes the modied functions that
-- can be used to create a stochastic LSystem

type PAlts a = [(Float, [a])]
type ProbRules a = [(Char, PAlts a)]
data ProbLSystem = ProbLSystem Float [Char] (ProbRules Char) deriving (Show)

-- Returns the rotation angle for the given LSystem.
angleP :: ProbLSystem -> Float
angleP (ProbLSystem theta _ _) = theta

-- Returns the axiom string for the given LSystem.
axiomP :: ProbLSystem -> [Char]
axiomP (ProbLSystem _ ax _) = ax

-- Returns the set of rules for the given LSystem.
rulesP :: ProbLSystem -> ProbRules Char
rulesP (ProbLSystem _ _ rls) = rls

-- Helper that returns a random number given
-- a standard generator
getRand :: StdGen -> (Float, StdGen)
getRand gen = (randNum, newgen)
  where
    (randNum, newgen) = random gen :: (Float, StdGen)

{-|
  Another version of lookupChar that
  chooses a binding with the following process:
  1. Choose a random number between [0,1]
  2. Start Adding the probabilities of each binding
      until the sum is in the range of the
      previous probability and the target probability
  3. Return the binding the corresponds to this probability
-}
lookupP :: ProbRules a -> Char -> StdGen -> ([a], StdGen)
lookupP rs searchVal gen = (randBind 0 justVals, newgen)
  where
    (Just justVals) = lookup searchVal rs

    (randNum, newgen) = getRand gen

    randBind :: Float -> PAlts a -> [a]
    randBind _ [(_,b)] = b
    randBind acc ((p,b):pbs)
      | randNum <= acc + p = b
      | otherwise          = randBind (acc+p) pbs

{-|
  Modified version of expandOne that keeps track of the
  standard generator used each time, so that the comand sequence generated
  is different each time.
-}
expandOneP :: ProbRules Char -> StdGen -> [Char] -> ([Char], StdGen)
expandOneP rls gen ss = go ss gen []
  where
    go [] ngen acc = (concat (reverse acc), ngen)
    go (s:ss) ngen acc =
      let
        (chunk, ngen') = lookupP rls s ngen
      in
        go ss ngen' (chunk:acc)

{-|
  Modified version of expand that keeps track of the
  standard generator used each time expandOneP is used.
-}
expandP :: StdGen -> ProbRules Char -> [Char] -> Int -> ([Char], StdGen)
expandP gen _   s 0 = (s, gen)
expandP gen rls s n = expandP gen1 rls s1 (n-1)
  where
    (s1, gen1) = expandOneP rls gen s

{-|
  Function to expand a probabilistic LSystem using
  a given seed so that each time the same shape is
  drawn given the same seed
-}
expandProbLSystemWithSeed :: ProbLSystem -> Int -> StdGen -> ([Command], StdGen)
expandProbLSystemWithSeed (ProbLSystem _ ax rls) n g0 = (parse s, g1)
  where
    (s, g1) = expandP g0 rls ax n

{-|
  Function that expands a probabilistic LSystem
  which is random and different every time, after
  the first expand on the axiom.
-}
expandProbLSystem :: ProbLSystem -> Int -> IO [Command]
expandProbLSystem pls n = do
  seed <- (randomIO :: IO Int)
  let (cmds, _g') = expandProbLSystemWithSeed pls n (mkStdGen seed)
  pure cmds

-- 3D LSystem:
------------------------------------------------------------------------------

-- This section includes the new functions, types and structured used to implement
-- 3D LSystems

type Vec3 = (Float, Float, Float)

-- Behaviour like addition
-- and multiplication for the 3d vectors
infixl 6 ^+^
infixl 7 *^

-- Adition of 2 3d vectors
(^+^) :: Vec3 -> Vec3 -> Vec3
(x1,y1,z1) ^+^ (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

-- Scale of a 3d vector by factor s
(*^) :: Float -> Vec3 -> Vec3
s *^ (x,y,z) = (s*x, s*y, s*z)

-- Find the midpoint of 2 3d vectors
midpoint :: Vec3 -> Vec3 -> Vec3
midpoint (x1,y1,z1) (x2,y2,z2) = ((x1+x2)/2, (y1+y2)/2, (z1+z2)/2)

-- Dot product of 2 3d vectors
dot :: Vec3 -> Vec3 -> Float
dot (x1,y1,z1) (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2

-- Cross product of 2 3d vectors
cross :: Vec3 -> Vec3 -> Vec3
cross (x1,y1,z1) (x2,y2,z2) =
  ( y1*z2 - z1*y2
  , z1*x2 - x1*z2
  , x1*y2 - y1*x2 )

{-|
  Function that returns the magnitude
  (length) of a 3d vector
-}
norm :: Vec3 -> Float
norm v = sqrt (dot v v)

{-|
  Function that returns the unit
  vector of a 3d vector
-}
unit :: Vec3 -> Vec3
unit v
  | n == 0    = (0,0,0)
  | otherwise = (1/n) *^ v
  where
    n = norm v

{-|
  Rotate vector v about 'axis' by angle (in degrees) using Rodrigues' formula.
  'axis' can be any non-zero vector and it is normalised in the function.

  https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula
-}
rotAxisDeg :: Vec3 -> Float -> Vec3 -> Vec3
rotAxisDeg axisDeg angDeg v = (c *^ v) ^+^ (s *^ kxv) ^+^ ((1 - c) * kv *^ k)
  where
    k   = unit axisDeg
    th  = degreesToRadians angDeg
    c   = cos th
    s   = sin th
    kv  = dot k v
    kxv = cross k v

-- Frame record data type that includes the forward,
-- right and upward direction
data Frame = Frame
  { fwd   :: Vec3  -- forward direction
  , right :: Vec3  -- right direction
  , up    :: Vec3  -- up direction
  } deriving (Show)

-- Turtle State is its position, orientation
-- and current colour
type TurtleState3 = (Vec3, Frame, Colour)

-- For the Rotation, I defined 3 helpers:
{-|
  Rotation by an angle ang around the up axis.
  fwd and right both rotate in the plane perpendicuar
  to up, while up stays fixed.
-}
rotateYaw :: Float -> Frame -> Frame
rotateYaw ang (Frame f r u) = Frame f' r' u
  where
    f' = rotAxisDeg u ang f
    r' = rotAxisDeg u ang r

{-|
  Rotation by an angle ang around the right axis.
  fwd and up both rotate in the plane perpendicuar
  to right, while right stays fixed.
-}
rotatePitch :: Float -> Frame -> Frame
rotatePitch ang (Frame f r u) = Frame f' r u'
  where
    f' = rotAxisDeg r ang f
    u' = rotAxisDeg r ang u

{-|
  Rotation by an angle ang around the fwd axis.
  up and right both rotate in the plane perpendicuar
  to fwd, while fwd stays fixed.
-}
rotateRoll :: Float -> Frame -> Frame
rotateRoll ang (Frame f r u) = Frame f r' u'
  where
    r' = rotAxisDeg f ang r
    u' = rotAxisDeg f ang u

-- Custom Command Structure for 3D
data Command3
  = F3                 -- move forward
  | YawL | YawR        -- turn left/right
  | PitchU | PitchD    -- tilt up/down
  | RollL | RollR      -- roll left/right
  | B3 [Command3]      -- branch block
  deriving (Show)

type Line3 = (Vec3, Vec3, Colour)

type Rules3 a = [(Char, [a])]

data LSystem3 = LSystem3 Float String (Rules3 Char) deriving (Show)

-- Return the generation angle given a 3d lsystem
angle3 :: LSystem3 -> Float
angle3 (LSystem3 a _ _) = a

-- Return the axiom given a 3d lsystem
axiom3 :: LSystem3 -> String
axiom3 (LSystem3 _ ax _) = ax

-- Return the rules given a 3d lsystem
rules3 :: LSystem3 -> Rules3 Char
rules3 (LSystem3 _ _ rs) = rs

{-|
  Function that looks for a binding of a
  character in the 3d ruleset given
  Pre: the binding exists
-}
lookupChar3 :: Rules3 a -> Char -> [a]
lookupChar3 rs ch = justval
  where
    (Just justval) = lookup ch rs

{-|
  Function that expands a string of characters
  once according to their 3d binding
-}
expandOne3 :: Rules3 Char -> String -> String
expandOne3 = concatMap . lookupChar3

{-|
  Function that expands a 3d ruleset into a string
  n times
-}
expand3 :: Rules3 Char -> String -> Int -> String
expand3 r s n = iterate (expandOne3 r) s !! n

{-|
  Function that parses a string ino a list of
  3d commands using my own "alphabet" structure.
  The patterns are comparible to the 2d structure
  but adjusted to 3d.
-}
parse3 :: String -> [Command3]
parse3 = fst . go
  where
    go :: String -> ([Command3], String)

    go [] = ([], [])

    go (']':cs) = ([], cs)

    go ('[':cs) = (B3 inner : rest, cs'')
      where
        (inner, cs')  = go cs
        (rest,  cs'') = go cs'

    go (c:cs) = (lookupC c ++ rest, cs')
      where
        (rest, cs') = go cs

    -- "Command map" for the 3d system
    -- The system's alphabet (which can
    -- be extended according to the user's needs)
    lookupC 'F' = [F3]
    lookupC '+' = [YawL]
    lookupC '-' = [YawR]
    lookupC '^' = [PitchU]
    lookupC '&' = [PitchD]
    lookupC '\\' = [RollL]
    lookupC '/'  = [RollR]
    lookupC  _   = []

{-|
  Function to expand and parse a 3d axiom
  into a Command3 n times.
-}
expandLSystem3 :: LSystem3 -> Int -> [Command3]
expandLSystem3 sys = parse3 . expand3 (rules3 sys) (axiom3 sys)

{-|
  Function identical to the move in 2d but changed so that
  it handles 3d vectors
-}
move3D :: Command3 -> Float -> TurtleState3 -> TurtleState3
move3D F3     ang (pos, fr, col) = (pos ^+^ fwd fr, fr, col)
move3D YawL   ang (p, fr, col)   = (p, rotateYaw   ang  fr, col)
move3D YawR   ang (p, fr, col)   = (p, rotateYaw (-ang) fr, col)
move3D PitchU ang (p, fr, col)   = (p, rotatePitch ang  fr, col)
move3D PitchD ang (p, fr, col)   = (p, rotatePitch (-ang) fr, col)
move3D RollL  ang (p, fr, col)   = (p, rotateRoll  ang  fr, col)
move3D RollR  ang (p, fr, col)   = (p, rotateRoll  (-ang) fr, col)

{-|
  Implementation of 3D trace function which has similar
  behaviour to the 2D trace 2 version with an explicit stack,
  starting from a certain position which is given
-}
trace3DFrom :: TurtleState3 -> [Command3] -> Float -> [Line3]
trace3DFrom start cmds ang = go cmds start []
  where
    go :: [Command3] -> TurtleState3 -> [([Command3], TurtleState3)] -> [Line3]
    go [] _ [] = []
    go [] _ ((next, st):stk) = go next st stk

    go (B3 block : rest) st@(pos, fr, col) stk =
      go block st ((rest, st):stk)

    go (F3 : rest) st@(pos, fr, col) stk =
      (pos, p2, drawCol) : go rest st' stk
      where
        st'@(p2, _, _) = move3D F3 ang st
        mid = midpoint pos p2
        drawCol = colourAt col mid

    go (cmd : rest) st stk =
      go rest (move3D cmd ang st) stk

{-|
  Intersect segment AB with the plane z = zPlane.
-}
intersectAtZ :: Float -> Vec3 -> Vec3 -> Vec3
intersectAtZ zP (x1,y1,z1) (x2,y2,z2) =
  (x1 + t*(x2-x1), y1 + t*(y2-y1), z1 + t*(z2-z1))
  where
    t = (zP - z1) / (z2 - z1)

{-|
  Fnction to project a single 3D point (x,y,z)
  with z > 0 to z = k' and drop z
-}
projectPoint :: Float -> Vec3 -> (Float,Float)
projectPoint k' (x,y,z) = (s*x, s*y)
  where
    s = k'/z

{-|
  Function to clip a segment to z >= eps.
  The Function returns Nothing if
  the "camera" is fully behind.
-}
clipFront :: Float -> (Vec3, Vec3) -> Maybe (Vec3, Vec3)
clipFront eps (a@(_,_,za), b@(_,_,zb))
  | za >= eps && zb >= eps = Just (a,b)
  | za <  eps && zb <  eps = Nothing
  | otherwise = Just (a', b')
  where
    a'
      | za >= eps = a
      | otherwise = intersectAtZ eps a b
    b'
      | zb >= eps = b
      | otherwise = intersectAtZ eps a b
      
{-|
  Function to project a 3D coloured segment with clipping
  eps: small positive z to clip against
  k' : viewing plane depth
  eps : clip plane (small epsilon value)
  gamma : contrast curve
-}
projectSeg :: Float -> Float -> Float -> (Vec3, Vec3, Colour) -> Maybe ColouredLine
projectSeg eps k' gamma (a@(x1,y1,z1), b@(x2,y2,z2), col) =
  case clipFront eps (a,b) of
    Nothing -> Nothing
    Just (a', b') -> Just ((ax, ay), (bx, by), col')
      where
        (ax, ay) = projectPoint k' a'
        (bx, by) = projectPoint k' b'
        zbar = 0.5 * (za + zb)
        f = clamp01 ((k' / zbar) ** gamma)
        (r,g,bc) = toRGB col
        col' = fromRGB (f*r, f*g, f*bc)

        (_,_,za) = a'
        (_,_,zb) = b'

{-| (Final function, I promise ...)
  Function to project a list of 3D lines
-}
projectLines3D :: Float -> Float -> Float -> [Line3] -> [ColouredLine]
projectLines3D eps k' gamma = mapMaybe (projectSeg eps k' gamma)

-- Helper Functions
------------------------------------------------------------------------------

degreesToRadians :: Float -> Float
degreesToRadians x = (x / 180) * pi

commandMap :: Rules Command
commandMap = [ ('M', [F])
             , ('N', [F])
             , ('X', [])
             , ('Y', [])
             , ('A', [])
             , ('+', [L])
             , ('-', [R])
             ]

{-|
  Function that Retuns a colour given a vector
  This function is used inside the trace function
  To make colours change smoothly and in a
  consistent manner.
  This function is almost identical to the
  "nextColour" function i used for the 2d system.
  However, i chose to implement it a different way
  for the 3d system.
-}
colourAt :: Colour -> Vec3 -> Colour
colourAt base (x,y,z) = fromRGB (r',g',b')
  where
    (r,g,b) = toRGB base
    (h,s,v) = rgbToHsv r g b

    -- ensure the colour is visible
    s'
      | s < 0.05  = 0.6
      | otherwise = s
    v'
      | v < 0.05  = 0.7
      | otherwise = v

    -- get the smooth transition around the area
    phi = 0.12*x + 0.09*y + 0.07*z

    -- how fast the colour changes
    delta = 0.50 * (0.5 + 0.5 * sin phi)

    h' = wrap01 (h + delta)
    (r',g',b') = hsvToRgb h' s' v'