//> using scala 3.3.1

trait Show[A] {
  def show(a: A): String
}
extension [A](value: A) def show(using show: Show[A]): String = show.show(value)

class MyUser(val name: String, val surname: String) {
  override def toString(): String = s"MyUser($name, $surname)"
}

case class MyUser2(name: String, surname: String)

final class SomeonesUser(val name: String, val surname: String)

@main def showExample(): Unit = {
  val myUser = new MyUser("John", "Smith")
  given Show[MyUser] = usr =>
    s"MyUser(name = ${usr.name}, surname = ${usr.surname})"

  val myUser2 = MyUser2("John", "Smith")
  given Show[MyUser2] = usr =>
    s"MyUser2(name = ${usr.name}, surname = ${usr.surname})"

  val someonesUser = new SomeonesUser("John", "Smith")
  given Show[SomeonesUser] = usr =>
    s"SomeonesUser(name = ${usr.name}, surname = ${usr.surname})"

  val myUserArr = Array(myUser)
  given Show[Array[MyUser]] = arr => s"Array(${arr.map(_.show).mkString(", ")})"

  println("MyUser:")
  println(s"toString: $myUser")
  println(s"show:     ${myUser.show}")
  println()
  println("MyUser2:")
  println(s"toString: $myUser2")
  println(s"show:     ${myUser2.show}")
  println()
  println("SomeonesUser:")
  println(s"toString: $someonesUser")
  println(s"show:     ${someonesUser.show}")
  println()
  println("Array[MyUser]:")
  println(s"toString: $myUserArr")
  println(s"show:     ${myUserArr.show}")
}
