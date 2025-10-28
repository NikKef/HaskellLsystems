module Examples where

import LSystems (LSystem(LSystem), ProbLSystem(ProbLSystem), LSystem3(LSystem3))

cross, triangle, arrowHead    :: LSystem
dragon, snowflake, tree, bush :: LSystem

cross = LSystem 90 "M-M-M-M" [ ('M', "M-M+M+MM-M-M+M")
                             , ('+', "+")
                             , ('-', "-")
                             ]

triangle = LSystem 90 "-M" [ ('M', "M+M-M-M+M")
                           , ('+', "+")
                           , ('-', "-")
                           ]

arrowHead = LSystem 60 "N" [ ('M', "N+M+N")
                           , ('N', "M-N-M")
                           , ('+', "+")
                           , ('-', "-")
                           ]

dragon = LSystem 45 "MX" [ ('M', "A")
                         , ('X', "+MX--MY+")
                         , ('Y', "-MX++MY-")
                         , ('A', "A")
                         , ('+', "+")
                         , ('-', "-")
                         ]

snowflake = LSystem 60 "M--M--M" [ ('M', "M+M--M+M")
                                 , ('+', "+")
                                 , ('-', "-")
                                 ]

tree = LSystem 45 "M" [ ('M', "N[-M][+M][NM]")
                      , ('N', "NN")
                      , ('[', "[")
                      , (']', "]")
                      , ('+', "+")
                      , ('-', "-")
                      ]

bush = LSystem 22.5 "X" [ ('X', "M-[[X]+X]+M[+MX]-X")
                        , ('M', "MM")
                        , ('[', "[")
                        , (']', "]")
                        , ('+', "+")
                        , ('-', "-")
                        ]

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
    -- Fs added create the "sphere" look)
  , ('u',"F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F+F")
  , ('d',"F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F-F")

    -- each petal rotates 30 degrees
  , ('t',"++")

  , ('F',"F"), ('+',"+"), ('-',"-"), ('^',"^"), ('&',"&"),
    ('[',"["), (']',"]")
  ]
