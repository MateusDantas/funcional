module Tipos where

{-
  Crie um tipo de dados chamado Shape que pode representar um circulo ou um retangulo.

  Você deve também criar as seguintes funções:

  createCircle:
  - recebe 3 floats x, y, r, onde x e y são as coordenadas do circulo e r é o raio.
  - retorna um Shape representando o circulo

  createRectangle:
  - recebe 4 floats x, y, x1, y1, onde x,y e x1,y1 são as duas coordenadas do retangulo.
  - retorna um Shape representando o retangulo

  area:
  - recebe um Shape
  - retorna a area desse shape


  Obs: Para o valor de PI, Haskell disponibiliza a constante 'pi'.
-}
data Point = Point Float Float
data Shape = Circle Point Float | Rectangle Point Point

createCircle x y r = Circle (Point x y) r

createRectangle x y x1 y1 = Rectangle (Point x y) (Point x1 y1)

area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
