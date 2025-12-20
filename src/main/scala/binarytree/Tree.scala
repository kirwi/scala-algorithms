package binarytree

enum Tree[+A]:
  case Empty
  case Node(value: A, left: Tree[A], right: Tree[A])