-- This module contains some utility functions
module Utilities where

import Model

pythagoreanTheorem :: Position -> Position -> Float
pythagoreanTheorem (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1-y2)^2)
