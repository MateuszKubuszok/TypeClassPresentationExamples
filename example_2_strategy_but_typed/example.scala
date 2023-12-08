//> using scala 3.3.1

trait Show[A] {
  def show(a: A): String
}

class MyUser(val name: String, val surname: String) {
  override def toString(): String = s"MyUser($name, $surname)"
}

case class MyUser2(name: String, surname: String)

final class SomeonesUser(val name: String, val surname: String)

def show[A](value: A)(show: Show[A]): String = show.show(value)

@main def showExample(): Unit = {
  val myUser = new MyUser("John", "Smith")
  val myUserShow: Show[MyUser] = usr =>
    s"MyUser(name = ${usr.name}, surname = ${usr.surname})"

  val myUser2 = MyUser2("John", "Smith")
  val myUser2Show: Show[MyUser2] = usr =>
    s"MyUser2(name = ${usr.name}, surname = ${usr.surname})"

  val someonesUser = new SomeonesUser("John", "Smith")
  val someonesUserShow: Show[SomeonesUser] = usr =>
    s"SomeonesUser(name = ${usr.name}, surname = ${usr.surname})"

  val myUserArr = Array(myUser)
  val myUserArrShow: Show[Array[MyUser]] = arr =>
    s"Array(${arr.map(myUserShow.show).mkString(", ")})"

  println("MyUser:")
  println(s"toString: $myUser")
  println(s"show:     ${show(myUser)(myUserShow)}")
  println()
  println("MyUser2:")
  println(s"toString: $myUser2")
  println(s"show:     ${show(myUser2)(myUser2Show)}")
  println()
  println("SomeonesUser:")
  println(s"toString: $someonesUser")
  println(s"show:     ${show(someonesUser)(someonesUserShow)}")
  println()
  println("Array[MyUser]:")
  println(s"toString: $myUserArr")
  println(s"show:     ${show(myUserArr)(myUserArrShow)}")
}
