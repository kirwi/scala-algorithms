package sorting

import org.scalatest.funsuite.AnyFunSuite

class MergeSortTest extends AnyFunSuite:

  test("sorts an empty list") {
    assertResult(Nil)(MergeSort.mergeSort[Int](Nil))
  }

  test("sorts a singleton") {
    assertResult(List(1))(MergeSort.mergeSort(List(1)))
  }

  test("sorts and already sorted list") {
    assertResult(List(1,2,3,4))(MergeSort.mergeSort(List(1,2,3,4)))
  }

  test("reverses a descending list") {
    assertResult(List(1,2,3,4))(MergeSort.mergeSort(List(4,3,2,1)))
  }

  test("sorts a list with duplicates") {
    assertResult(List(1,2,3,3,4))(MergeSort.mergeSort(List(3,1,2,4,3)))
  }

  test("sorts a list of strings") {
    val (apple, banana, pear) = ("apple", "banana", "pear")
    assertResult(List(apple, banana, pear))(MergeSort.mergeSort(List(banana, pear, apple) ))
  }

  test("sorts a list of custom objects") {
    case class Person(name: String, age: Int)
    given Ordering[Person] = Ordering.by[Person, Int](_.age).orElseBy(_.name)
    val people = List(Person("Alice", 30), Person("Bob", 20), Person("Carl", 50), Person("Zelda", 30))
    assertResult(
      List(
        Person("Bob", 20),
        Person("Alice", 30),
        Person("Zelda", 30),
        Person("Carl", 50)
      )
    )(
      MergeSort.mergeSort(people)
    )
  }
