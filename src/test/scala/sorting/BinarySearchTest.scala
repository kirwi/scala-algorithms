package sorting

import org.scalatest.funsuite.AnyFunSuite
import searching.BinarySearch
import Ordering.Implicits.*

class BinarySearchTest extends AnyFunSuite:

  val fibsList = List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
  val fibsVector = Vector(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)

  test("Relaxed binary search can find the index of an element in a list") {
    assertResult(Some(5))(BinarySearch.search(fibsList, 8))
  }

  test("Relaced binary search returns None when no target is found") {
    assertResult(None)(BinarySearch.search(fibsList, 11))
  }

  test("Iterable binary search can find a target index") {
    assertResult(Some(5))(BinarySearch.searchIter(fibsVector, 8))
  }

  test("Iterable binary search returns None when no target is found") {
    assertResult(None)(BinarySearch.searchIter(fibsVector, 11))
  }

  test("searchFirst finds the smallest index matching the target") {
    assertResult(Some(0))(BinarySearch.searchFirst(fibsVector, 1))
  }

  test("searchLast find the largest index matching the target") {
    assertResult(Some(1))(BinarySearch.searchLast(fibsVector, 1))
  }
  
  test("lowerBound finds the index of the smallest element larger than the target") {
    assertResult(4)(BinarySearch.lowerBound(fibsVector, 4))
    assertResult(0)(BinarySearch.lowerBound(fibsVector, 1))
    assertResult(fibsVector.length)(BinarySearch.lowerBound(fibsVector, 60))
  }
