package binarytree

import Tree.{Empty, Node}

object TreeAlgorithms:
  def inorder[A](tree: Tree[A]): List[A] = tree match
    case Empty => Nil
    case Node(v, l, r) => inorder(l) ++ (v :: inorder(r))

  def preorder[A](tree: Tree[A]): List[A] = tree match
    case Empty => Nil
    case Node(v, l, r) => (v :: preorder(l)) ++ preorder(r)

  def postorder[A](tree: Tree[A]): List[A] = tree match
    case Empty => Nil
    case Node(v, l, r) => postorder(l) ++ postorder(r) ++ List(v)

  def size[A](tree: Tree[A]): Int = tree match
    case Empty => 0
    case Node(v, l, r) => 1 + size(l) + size(r)

  def height[A](tree: Tree[A]): Int = tree match
    case Empty => 0
    case Node(v, l, r) => 1 + height(l) max height(r)

  def leafs[A](tree: Tree[A]): Int = tree match
    case Empty => 0
    case Node(v, l, r) => (l, r) match
      case (Empty, Empty) => 1
      case _ => leafs(l) + leafs(r)

  def symmetric[A](tree: Tree[A]): Boolean = tree match
    case Empty => true
    case Node(v, l, r) => (l, r) match
      case (Node(_, _, _), Node(_, _, _)) => symmetric(l) && symmetric(r)
      case _ => false

  def balanced[A](tree: Tree[A]): Boolean =
    def go(t: Tree[A]): (Boolean, Int) = t match
      case Empty => (true, 0)
      case Node(v, l, r) =>
        val (lBalanced, lHeight) = go(l)
        val (rBalanced, rHeight) = go(r)
        val balancedHere = lBalanced && rBalanced && math.abs(lHeight - rHeight) <= 1
        (balancedHere, lHeight max rHeight)

    go(tree)._1

  def nodeCount[A](tree: Tree[A]): Int = tree match
    case Empty => 0
    case Node(v, l, r) => (l, r) match
      case (Empty, Empty) => 0
      case _ => 1 + nodeCount(l) + nodeCount(r)

  def mirror[A](tree: Tree[A]): Tree[A] = tree match
    case Empty => Empty
    case Node(v, l, r) => Node(v, mirror(r), mirror(l))

  def equal[A](t1: Tree[A], t2: Tree[A]): Boolean = (t1, t2) match
    case (Empty, Empty) => true
    case (Node(v1, l1, r1), Node(v2, l2, r2)) =>
      (v1 == v2) && equal(l1, l2) && equal(r1, r2)
    case _ => false
  
  def subtree[A](t1: Tree[A], t2: Tree[A]): Boolean = (t1, t2) match
    case (_, Empty) => true
    case (Empty, Node(_, _, _)) => false
    case (Node(_, l, r), _) =>
      equal(t1, t2) || subtree(l, t2) || subtree(r, t2)

  def diameter[A](tree: Tree[A]): Int =
    def go(t: Tree[A]): (Int, Int) = t match // (diameter, height)
      case Empty => (0, 0)
      case Node(_, l, r) =>
        val (dl, hl) = go(l)
        val (dr, hr) = go(r)
        (dl max dr max (1 + hl + hr), hl max hr)

    go(tree)._1

  def paths[A](tree: Tree[A]): List[List[A]] = tree match
    case Empty => Nil
    case Node(v, l, r) => (l, r) match
      case (Empty, Empty) => List(List(v))
      case _ => paths(l).map(v :: _) ++ paths(r).map(v :: _)

  def maxPathSum(tree: Tree[Int]): Int =
    def go(t: Tree[Int]): (Int, Int) = t match
      case Empty => (Int.MinValue, Int.MinValue)
      case Node(v, l, r) =>
        val (fullL, downL) = go(l)
        val (fullR, downR) = go(r)
        val maxDownHere = v max (downL + v) max (downR + v)
        val maxFullHere = fullL max fullR max (v + downL + downR)
        (maxFullHere, maxDownHere)

    go(tree)._1

  def hasPathSum(tree: Tree[Int], target: Int): Boolean = tree match
    case Empty => false
    case Node(v, Empty, Empty) => v == target
    case Node(v, l, r) => hasPathSum(l, target - v) || hasPathSum(r, target - v)

  def allSumPaths(tree: Tree[Int], target: Int): List[List[Int]] =
    def go(t: Tree[Int], currTarget: Int, acc: List[Int]): List[List[Int]] = t match
      case Empty => Nil
      case Node(v, Empty, Empty) => if v == currTarget then List(v :: acc).reverse else Nil
      case Node(v, l, r) =>
        val updatedAcc = v :: acc
        go(l, currTarget - v, updatedAcc) ++ go(r, currTarget - v, updatedAcc)

    go(tree, target, Nil)

  def countPaths(tree: Tree[Int], target: Int): Int =
    def go(t: Tree[Int], tar: Int): Int = t match
      case Empty => 0
      case Node(v, l, r) =>
        (if v == tar then 1 else 0) + go(l, tar - v) + go(r, tar - v)

    tree match
      case Empty => 0
      case Node(v, l, r) => go(tree, target) + countPaths(l, target) + countPaths(r, target)

  def lowestCommonAncestor[A](tree: Tree[A], p: A, q: A): Option[A] =
    def go(t: Tree[A], p: A, q: A): (Option[A], Boolean, Boolean) = t match
      case Empty => (None, false, false)
      case Node(v, l, r) =>
        val (lcaLeft, pFoundLeft, qFoundLeft) = go(l, p, q)
        val (lcaRight, pFoundRight, qFoundRight) = go(r, p, q)
        val pFound = pFoundLeft || pFoundRight || v == p
        val qFound = qFoundLeft || qFoundRight || v == q
        lcaLeft.orElse(lcaRight) match
          case None =>
            if pFound && qFound then (Some(v), pFound, qFound)
            else (None, pFound, qFound)
          case Some(lca) => (Some(lca), pFound, qFound)

    go(tree, p, q)._1

  def serialize[A](tree: Tree[A]): String =
    def preorder(t: Tree[A], acc: List[String]): List[String] = t match
      case Empty =>  "#" :: acc
      case Node(v, l, r) => v.toString :: preorder(l, preorder(r, acc))

    preorder(tree, Nil).mkString(",")

  def deserialize[A](s: String, f: String => A): Tree[A] =
    def go(xs: List[String]): (Tree[A], List[String]) = xs match
      case y :: ys =>
        if y == "#" then (Empty, ys)
        else
          val (left, rl) = go(ys)
          val (right, rr) = go(rl)
          (Node(f(y), left, right), rr)
      case Nil => (Empty, Nil)

    go(s.split(",").toList)._1

  private def partitionByRoot[A](xs: List[A], root: A): (List[A], List[A], Int) =
    def go(xs: List[A]): (List[A], List[A], Int) = xs match
      case Nil => (Nil, Nil, 0)
      case y :: ys =>
        if y == root then (Nil, ys, 0)
        else
          val (l, r, len) = go(ys)
          (y :: l, r, len + 1)

    go(xs)

  private def partitionByLength[A](xs: List[A], len: Int): (List[A], List[A]) = xs match
    case Nil => (Nil, Nil)
    case y :: ys =>
      if len > 0 then
        val (l, r) = partitionByLength(ys, len - 1)
        (y :: l, r)
      else (Nil, xs)


  def inorderPostorder[A](inorder: List[A], postorder: List[A]): Tree[A] = inorder match
    case Nil => Empty
    case x :: Nil => Node(x, Empty, Empty)
    case _ =>
      val root = postorder.last
      val (inLeft, inRight, len) = partitionByRoot(inorder, root)
      val (postLeft, postRight) = partitionByLength(postorder, len)
      Node(root, inorderPostorder(inLeft, postLeft), inorderPostorder(inRight, postRight.init))

  def inorderPreorder[A](inorder: List[A], preorder: List[A]): Tree[A] = inorder match
    case Nil => Empty
    case x :: Nil => Node(x, Empty, Empty)
    case _ =>
      val root :: rest = preorder: @unchecked
      val (inLeft, inRight, len) = partitionByRoot(inorder, root)
      val (preLeft, preRight) = partitionByLength(rest, len)
      Node(root, inorderPreorder(inLeft, preLeft), inorderPreorder(inRight, preRight))


