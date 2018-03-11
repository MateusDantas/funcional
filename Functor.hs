module Functor where

{-
  Dado o tipo abaixo que representa uma binary tree, implemente a função descrita abaixo.

  multiplyByTwo:
  - recebe uma BinaryTree
  - retorna uma BinaryTree com os valores de todos os seus nós multiplicados por 2
  - Obs: Utilize fmap para resolver esse problema.
-}
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a) deriving (Eq, Show)

instance Functor BinaryTree where
  fmap f NIL = NIL
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

multiplyByTwo tree = fmap (*2) tree
