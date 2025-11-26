package sorting

import org.scalatest.funsuite.AnyFunSuite

class QuickSortTest extends AnyFunSuite:

  case class Person(name: String, age: Int)

  given Ordering[Person] = Ordering.by(_.age)

  val kyle = Person("Kyle", 44)
  val ness = Person("Ness", 33)
  val corey = Person("Corey", 33)
  val moco = Person("Moco", 14)
  val fonzi = Person("Fonzi", 33)
  val people = List(kyle, ness, corey, moco, fonzi)

  test("stable quick sort preserves original order") {
    case class X(id: String, value: Int)
    given Ordering[X] = Ordering.by(_.value)
    val xs = List(X("a", 3), X("b", 3), X("c", 3))
    assertResult(List("a", "b", "c"))(QuickSort.sortStable(xs).map(_.id))
  }

  test("stable quick sort preserves original order with degenerate and non-degenerate values") {
    assertResult(List("Moco", "Ness", "Corey", "Fonzi", "Kyle"))(QuickSort.sortStable(people).map(_.name))
  }

  test("foldRight version of quickSort is stable") {
    assertResult(List("Moco", "Ness", "Corey", "Fonzi", "Kyle"))(QuickSort.foldSort(people).map(_.name))
  }


