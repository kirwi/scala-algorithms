package binarytree

import org.scalacheck.{Gen, Arbitrary}
import binarytree.Tree
import binarytree.Tree.{Empty, Node}

object TreeGen:
  def genTree(maxDepth: Int): Gen[Tree[Int]] =
    if maxDepth <= 0 then Gen.const(Empty)
    else
      Gen.oneOf(
        Gen.const(Empty),
        for
          v <- Gen.choose(-10, 10)
          left <- genTree(maxDepth - 1)
          right <- genTree(maxDepth - 1)
        yield Node(v, left, right)
      )

  val genSizedTree: Gen[Tree[Int]] =
    Gen.sized { size =>
      val depth = (size / 3).max(2)
      genTree(depth)
    }

  given Arbitrary[Tree[Int]] = Arbitrary(genSizedTree)
