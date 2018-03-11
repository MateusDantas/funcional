module Monads where

{-
  Dado os tipos abaixo que representa um robô em um espaço de 2 dimensões e os possiveis movimentos
  que o robo pode fazer, implemente as funções especificadas abaixo utilizando a monad Maybe.

  LEFT: Movimenta o robo de uma posição (x,y) para (x-1,y)
  RIGHT: Movimenta o robo de uma posição (x,y) para (x+1,y)
  UP: Movimenta o robo de uma posição (x,y) para (x,y+1)
  DOWN: Movimenta o robo de uma posição (x,y) para (x,y-1)

  move:
  - recebe um Move e um Robot
  - retorna uma monad com a nova posição do robô de acordo com o movimento.
  - retorna Nothing se o robô ultrapassar o tabuleiro. Imagine que o tabuleiro é definido entre as posições (0,0) e (5,5) inclusivo.

  applyMoves:
  - recebe um Robot e uma lista de Move
  - retorna uma monad com a nova posição do robô após aplicar os movimentos da lista em sequência (do primeiro para o último).
  - retorna Nothing se o robô ultrapassar o tabuleiro. Imagine que o tabuleiro é definido entre as posições (0,0) e (5,5) inclusivo.
  - Obs: Implementar applyMoves usando foldl
-}
data Robot = Robot Int Int deriving (Eq, Show)
data Move = LEFT | RIGHT | UP | DOWN

moveLeft (Robot x y)
  | x - 1 < 0 = Nothing
  | otherwise = Just (Robot (x - 1) y)

moveRight (Robot x y)
  | x + 1 > 5 = Nothing
  | otherwise = Just (Robot (x + 1) y)

moveUp (Robot x y)
  | y + 1 > 5 = Nothing
  | otherwise = Just (Robot x (y + 1))

moveDown (Robot x y)
  | y - 1 < 0 = Nothing
  | otherwise = Just (Robot x (y - 1))

move LEFT robot = moveLeft robot
move RIGHT robot = moveRight robot
move UP robot = moveUp robot
move DOWN robot = moveDown robot

applyMove maybeRobot mv = maybeRobot >>= move mv

applyMoves robot moves = foldl (applyMove) (Just robot) moves
