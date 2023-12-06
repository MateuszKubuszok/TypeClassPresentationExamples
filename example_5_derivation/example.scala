//> using scala 3.3.1
//> using dep com.softwaremill.magnolia1_3::magnolia:1.3.4

import magnolia1.*

trait Show[A] {
  def show(a: A): String
}
object Show extends AutoDerivation[Show] {

  given Show[String] = str => '"' + str + '"'
  given Show[Int] = int => int.toString 

  given [A](using A: Show[A]): Show[Array[A]] = arr => s"Array(${arr.map(_.show).mkString(", ")})"

  def join[A](ctx: CaseClass[Show, A]): Show[A] = a => {
    val params = ctx.params.map { param =>
      val name = param.label
      val value = param.deref(a).show(using param.typeclass)
      s"$name = $value"
    }
    ctx.typeInfo.short + (if (params.isEmpty) "" else s"(${params.mkString(", ")})")
  }
  
  def split[A](ctx: SealedTrait[Show, A]): Show[A] = a => {
    ctx.choose(a) { subtype =>
      subtype.value.show(using subtype.typeclass)
    }
  }
}
extension [A](value: A)
  def show(using show: Show[A]): String = show.show(value)

case class MyUser(name: String, surname: String)

@main def showExample(): Unit = {
  println("(String, Int)")
  println(("John", 21).show)
  println()
  println("(String, String, Int)")
  println(("John", "Smith", 21).show)
  println()
  println("Array[(String, String, Int)]")
  println(Array(("John", "Smith", 21)).show)
  println()
  println("MyUser")
  println(Array(MyUser("John", "Smith")).show)
}