module Examples where

import LSystems (LSystem(LSystem), ProbLSystem(ProbLSystem), LSystem3(LSystem3))

-- Bush LSystem which implements probabilistic rewriting
probBush :: ProbLSystem
probBush = ProbLSystem 22.5 "X"
                                [ ('X', [(0.45, "M-[[X]+X]+M[+MX]-X")
                                        ,(0.55, "M[+X]M[-X]MX")])
                                , ('M', [(1.0, "MM")])
                                , ('+', [(1.0, "+")]), ('-', [(1.0, "-")])
                                , ('[', [(1.0, "[")]), (']', [(1.0, "]")])
                                ]

-- This was supposed to be something similar to the 2d
-- Tree, which is not but it is still cool!
tree3D :: LSystem3
tree3D = LSystem3 45 "F"
  [ ('F', "N[\\-F][/+F][^F]")
  , ('N', "FF")
  , ('[', "[")
  , (']', "]")
  , ('+', "+")
  , ('-', "-")
  , ('^', "^")
  , ('&', "&")
  , ('\\', "\\")
  , ('/', "/")
  ]

-- This is my submition to the LSystems competition
-- It took A LOT of trial and error and i finally ended up
-- with this implementation of 3d-like petals
-- constructing a sphere.
mandala3D :: LSystem3
mandala3D = LSystem3 15 "A"
  [ 
    -- Create 12 petals and then stop.
    ('A',"PtB"), ('B',"PtC"), ('C',"PtD"), ('D',"PtE"),
    ('E',"PtF"), ('F',"PtG"), ('G',"PtH"), ('H',"PtI"),
    ('I',"PtJ"), ('J',"PtK"), ('K',"PtL"), ('L',"PtM"),
    ('M',"")

    -- one petal consists of 3 arcs
  , ('P',"[U][V][gU][gV][ggU][ggV]")

  -- the unit radius
  , ('g',"F")

  -- the half-arcs are lifted to make it feel 3d
  , ('U',"^u&")
  , ('V',"^d&")

    -- unit arcs which (with the large amount of
    -- Fs added create the "shere" look)
  , ('u',"F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F")
  , ('d',"F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F")

    -- each petal rotates 30 degrees
  , ('t',"++")

  , ('F',"F"), ('+',"+"), ('-',"-"), ('^',"^"), ('&',"&"),
    ('[',"["), (']',"]")
  ]