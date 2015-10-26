import scalaz._
import Scalaz._

sealed trait TList[+A]
case object TNil extends TList[Nothing]
case class TCons[+A](head: A, tail: TList[A]) extends TList[A]

object TList {
  def apply[A](as: A*): TList[A] = as match {
    case Nil => TNil
    case _ => TCons(as.head, apply(as.tail: _*))
 }
}

sealed trait Animal {
  def printName(): String
}

case class Cat(name: String) extends Animal {
  def printName(): String = s"Name is $name"
}

case class Dog(name: String) extends Animal {
  def printName(): String = s"Name is $name"
}

object scala_test {

  def lists(): Unit = {
    println("lists\n")
    val fl =  List(1,0,0,1,0,1).foldLeft(List[Int]())(
      (list, x) => (list.headOption.getOrElse(0) + x) :: list
    ).reverse
    println(s"foldLeft: $fl")

    val fr = List(1,0,0,1,0,1).foldRight(List[Int]())(
      (x, list) => x + list.headOption.getOrElse(0) :: list )
    println(s"foldRight: $fr")

    val (left, right) = List(1,2,3,4).partition(_ < 3)
    println()
  }

  def maps(): Unit = {
    println("maps\n")
    val (left, right) = Map("a" -> 1, "b" -> 2, "c" -> 3).partition{ case (x,y) => y < 3 }
    println(s"left: $left, right: $right")
  }

  // multiplte parameter lists
  def f(x: Int)(y: Int): Int = x * y

  def main(args: Array[String]): Unit = {
    println("Hello World!")

    lists()
    maps()

    val mulByThree: Int => Int = f(3)
    val x = 5
    println(s"3 * $x: ${mulByThree(x)}")

    def greet(animal: Animal) = animal match {
      case Cat(name) => s"Hello $name"
      case Dog(name) => s"Greetings $name"
    }

    greet(Cat("Fiz"))

    println(TList(1,2))
  }
}
