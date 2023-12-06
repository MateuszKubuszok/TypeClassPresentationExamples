//> using scala 3.3.1

trait Show[A] {
  def show(a: A): String
}
object Show {

  given Show[String] = str => '"' + str + '"'
  given Show[Int] = int => int.toString

  given [A](using A: Show[A]): Show[Array[A]] = arr =>
    s"Array(${arr.map(_.show).mkString(", ")})"

  given [A, B](using A: Show[A], B: Show[B]): Show[(A, B)] = { case (a, b) =>
    s"Tuple2(${a.show}, ${b.show})"
  }

  given [A, B, C](using A: Show[A], B: Show[B], C: Show[C]): Show[(A, B, C)] = {
    case (a, b, c) => s"Tuple3(${a.show}, ${b.show}, ${c.show})"
  }
}
extension [A](value: A) def show(using show: Show[A]): String = show.show(value)

@main def showExample(): Unit = {
  println("(String, Int)")
  println(("John", 21).show)
  println()
  println("(String, String, Int)")
  println(("John", "Smith", 21).show)
  println()
  println("Array[(String, String, Int)]")
  println(Array(("John", "Smith", 21)).show)
}
