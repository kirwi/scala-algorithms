package datastructures

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import datastructures.Tree
import datastructures.Tree.{Empty, Node}
import algorithms.TreeAlgorithms
import algorithms.TreeAlgorithms.*

class TreeAlgorithmsPropertySpec
  extends AnyFunSuite
  with Matchers
  with ScalaCheckDrivenPropertyChecks:
  
  import TreeGen.given


  test("Can generate trees") {
    forAll { (t: Tree[Int]) =>
      succeed
    }
  }
  
  test("Traversal lengths equal tree size") {
    forAll { (t: Tree[Int]) =>
      inorder(t).length shouldBe TreeAlgorithms.size(t)
    }
  }