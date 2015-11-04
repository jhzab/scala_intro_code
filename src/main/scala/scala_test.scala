import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import doobie.imports._
import java.sql.SQLException

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
  def fac(x: Int): Int = {
    @annotation.tailrec
    def go(acc: Int, x: Int): Int = x match {
      case 0 => acc
      case i => go(acc * x, x - 1)
    }

    go(1, x)
  }

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

  private[this] val xa = DriverManagerTransactor[Task](
    "org.h2.Driver", "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "", ""
  )

  def createSchema(): Int =
    sql"CREATE SCHEMA TESTDB".update.run.transact(xa).run

  def createTables() =
    (List(
      sql"""CREATE TABLE IF NOT EXISTS TESTDB.TEST1(TEST VARCHAR(5) NOT NULL, VALUE DOUBLE NOT NULL, PRIMARY KEY (TEST, VALUE))""",
      sql"""CREATE TABLE IF NOT EXISTS TESTDB.TEST2(TEST VARCHAR(5) NOT NULL, VALUE DOUBLE NOT NULL, PRIMARY KEY (TEST, VALUE))"""
    ).traverseU(_.update.run): ConnectionIO[List[Int]]).attemptSql.transact(xa).run

  def insertData(data: Map[String, Double]): \/[SQLException, List[Int]] = {
    def buildStatement(test: String, value: Double) =
      sql"""INSERT INTO TESTDB.TEST1 (TEST, VALUE) VALUES ($test, $value)"""

    (data.map{
        case (test, value) => buildStatement(test, value)
      }.toList.traverseU(_.update.run): ConnectionIO[List[Int]]
    ).attemptSql.transact(xa).run
  }

  case class TestData(test: String, value: Double)

  def selectData = {
    sql"SELECT TEST, VALUE FROM TESTDB.TEST1".query[TestData].list.transact(xa).attemptSql.run
  }

  def printRet[A](r: A): Unit = r match {
      case \/-(l) => println(s"Success: $l")
      case -\/(error) => println(s"Failure: $error")
    }

  def main(args: Array[String]): Unit = {
    //lists()
    //maps()
    val mulByThree: Int => Int = f(3)
    val x = 5
    // println(s"3 * $x: ${mulByThree(x)}")

    def greet(animal: Animal) = animal match {
      case Cat(name) => s"Hello $name"
      case Dog(name) => s"Greetings $name"
    }
    // greet(Cat("Fiz"))
    // println(s"Factorial: ${fac(4)}")
    // println(TList(1,2))

    createSchema()
    //createSchema()
    createTables() match {
      case \/-(l) => println("Successfull SQL statements: " + l.size)
      case -\/(error) => println(error)
    }

    val add1 = insertData(Map("test1" -> 2, "test2" -> 10000))
    printRet(add1)

    val add2 = insertData(Map("test12345" -> 2, "test2" -> 10000))
    printRet(add2)

    printRet(selectData)
  }
}
