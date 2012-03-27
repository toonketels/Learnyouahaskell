module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidVolume
) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume a = a ^ 3

cubeArea :: Float -> Float
cubeArea a = 6 * (a ^ 2)

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume h w l = h * l * w
