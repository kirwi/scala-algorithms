import sorting.QuickSort
import sorting.MergeSort

@main def playground(): Unit =
  val xs = List(69, 420, 5, 37, 11, 44, 33, 69, 420, 11)
  println(s"${xs} sorted by mergeSort: ${MergeSort.mergeSort(xs)}")
  println(s"${xs} sorted by quickSort: ${QuickSort.sort(xs)}")
  println(s"${xs} sorted by quickSortStable: ${QuickSort.sortStable(xs)}")

  case class Person(name: String, age: Int)
  given Ordering[Person] = Ordering.by(_.age)

  val kyle = Person("Kyle", 44)
  val ness = Person("Ness", 33)
  val corey = Person("Corey", 33)
  val moco = Person("Moco", 14)
  val fonzi = Person("Fonzi", 33)

  val people = List(kyle, ness, corey, moco, fonzi)
  println(s"people sorted by mergeSort: ${MergeSort.mergeSort(people)}")
  println(s"people sorted by quickSort: ${QuickSort.sort(people)}")
  println(s"people sorted by quickSortStable: ${QuickSort.sortStable(people)}")
