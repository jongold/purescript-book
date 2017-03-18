module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Unit (Unit)
import Global (encodeURIComponent)
import Math (pi, sqrt)
import Prelude ((+), (*), bind)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

-- Use the Math.pi constant to write a function circleArea
-- which computes the area of a circle with a given radius.
circleArea :: Number -> Number
circleArea r = pi * r * r

-- Use bower install to install the purescript-globals package
-- as a dependency. Test out its functions in PSCi
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow (diagonal 3.0 4.0)
  logShow (circleArea 5.0)
  logShow (encodeURIComponent "foo?bar=123")
