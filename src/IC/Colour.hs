module IC.Colour where

data Colour = Black | White
            | Red | Green | Blue
            | Cyan | Magenta | Yellow
            | Custom Float Float Float
            deriving (Show, Eq, Ord)


{-|
  Function that returns the "next" colour
  using the oredering of the colour wheel
  for it to be "smooth".

  Formulas for conversion and information on RGB/HSV were sourced from:
  https://www.rapidtables.com/convert/color/rgb-to-hsv.html
  https://www.rapidtables.com/convert/color/hsv-to-rgb.html
-}

-- | Smooth hue rotation step
hueStep :: Float
hueStep = 0.075

nextColour :: Colour -> Colour
nextColour c = Custom r' g' b'
  where
    (r,g,b) = toRGB c
    (h,s,v) = rgbToHsv r g b

    -- If the colour is grey, white or black
    -- add some saturation so the wheel is visible.
    s'
      | s < 0.05 = 0.6
      | otherwise = s
    v'
      | v < 0.05 = 0.6
      | otherwise = v

    h' = wrap01 (h + hueStep)

    (r',g',b') = hsvToRgb h' s' v'

-- Helpers

toRGB :: Colour -> (Float, Float, Float)
toRGB Black         = (0, 0, 0)
toRGB White         = (1, 1, 1)
toRGB Red           = (1, 0, 0)
toRGB Yellow        = (1, 1, 0)
toRGB Green         = (0, 1, 0)
toRGB Cyan          = (0, 1, 1)
toRGB Blue          = (0, 0, 1)
toRGB Magenta       = (1, 0, 1)
toRGB (Custom r g b)= (clamp01 r, clamp01 g, clamp01 b)

clamp01 :: Float -> Float
clamp01 x | x < 0     = 0
          | x > 1     = 1
          | otherwise = x

wrap01 :: Float -> Float
wrap01 x = x - k
  where
    k = fromIntegral (floor x)

rgbToHsv :: Float -> Float -> Float -> (Float, Float, Float)
rgbToHsv r g b = (h, s, v)
  where
    mx = max r (max g b)
    mn = min r (min g b)

    d = mx - mn
    v = mx

    s
      | mx == 0   = 0
      | otherwise = d / mx

    hRaw
      | d == 0    = 0
      | mx == r   = ((g - b) / d) / 6
      | mx == g   = ((b - r) / d) / 6 + (1/3)
      | otherwise = ((r - g) / d) / 6 + (2/3)

    h = wrap01 hRaw

hsvToRgb :: Float -> Float -> Float -> (Float, Float, Float)
hsvToRgb h s v = (r1 + m, g1 + m, b1 + m)
  where
    c = v * s
    h' = h * 6
    i = floor h'
    f = h' - fromIntegral i
    x = c * (1 - abs (2*f - 1))

    (r1,g1,b1) =
      case i `mod` 6 of
        0 -> (c, x, 0)
        1 -> (x, c, 0)
        2 -> (0, c, x)
        3 -> (0, x, c)
        4 -> (x, 0, c)
        _ -> (c, 0, x)
        
    m = v - c

fromRGB :: (Float,Float,Float) -> Colour
fromRGB (r,g,b) = Custom r g b