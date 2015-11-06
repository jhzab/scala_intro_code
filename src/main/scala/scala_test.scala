import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import doobie.imports._
import java.sql.SQLException
import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

//import org.http4s._
import org.http4s.dsl._
import org.http4s.server.HttpService
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.headers.`Content-Type`
import org.http4s.MediaType._

sealed trait TList[+A]
case object TNil extends TList[Nothing]
case class TCons[+A](head: A, tail: TList[A]) extends TList[A]

object RESTApi {
  def service(implicit executionContext: ExecutionContext) =
    HttpService {
      case req @ GET -> Root / "ping" =>
        Ok(s"pong. Parameters: ${req.params}")
          .putHeaders(`Content-Type`(`text/plain`))
    }
}

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

case class TestData(test: String, value: Double)

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
  def fparam(x: Int)(y: Int): Int = x * y

  def easyExamples(): Unit = {
    lists()
    maps()
    val mulByThree: Int => Int = fparam(3)
    val x = 5
    println(s"3 * $x: ${mulByThree(x)}")

    def greet(animal: Animal) = animal match {
      case Cat(name) => s"Hello $name"
      case Dog(name) => s"Greetings $name"
    }
    greet(Cat("Fiz"))
    println(s"Factorial: ${fac(4)}")
    println(TList(1,2))
  }

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

  def insertMany(data: List[TestData]): \/[SQLException, Int] = {
    val sql = "INSERT INTO TESTDB.TEST1 (TEST, VALUE) VALUES (?, ?)"

    Update[TestData](sql).updateMany(data).attemptSql.transact(xa).run
  }

  def selectData() = {
    sql"SELECT TEST, VALUE FROM TESTDB.TEST1".query[TestData].list.transact(xa).attemptSql.run
  }

  def printRet[A](r: A): Unit = r match {
      case \/-(l) => println(s"Success: $l")
      case -\/(error) => println(s"Failure: $error")
  }

  def updateData() = {
    val sql = sql"""UPDATE TESTDB.TEST1 SET VALUE = 123 WHERE VALUE IS 2"""

    sql.update.withGeneratedKeys[TestData]("test", "value").list.transact(xa).run
  }

  def f(x: Int):Future[Int] = Future {
    if (x > 0 && x < 1000) {
      Thread.sleep(x)
      x
    } else
      0
  }

  def g(x: Int): Future[Option[Int]] = Future {
    x match {
      case x if x < 0 => none
      case x if x > 1000 => some(x)
      case x => Thread.sleep(x); some(x)
    }
  }

  def main(args: Array[String]): Unit = {
    def startWebServer(): Unit = {
      BlazeBuilder.bindHttp(8080)
        .mountService(RESTApi.service, "/")
        .run
        .awaitShutdown()
    }

    args.toList match {
      case Nil => println("Not starting webserver.")
      case x::xs if x == "http4s" => startWebServer()
    }
    // Future
    val futureTest = for { a <- f(100); b <- f(300) } yield a + b
    Await.result(futureTest, Duration.Inf)

    val optionTTest = for {
      a <- OptionT(g(300));
      b <- OptionT(g(2000));
      c <- OptionT(g(-1))
    } yield a + b + c

    Await.result(optionTTest.run, Duration.Inf)


    createSchema()
    //createSchema()
    printRet(createTables())

    val insert1 = insertData(Map("test1" -> 2, "test2" -> 10000))
    printRet(insert1)

    val insert2 = insertData(Map("test3" -> 23, "testTooLong" -> 10000, "test4" -> 1))
    printRet(insert2)

    val data = Map("test5" -> 5, "test6TooLong" -> 6, "test7" -> 7)

    val insert3 = insertMany(data.map{ case (k,v) => TestData(k, v) }.toList)
    printRet(insert3)

    printRet(selectData)
    print(updateData)
    printRet(selectData)
  }
}

/*
// DOOBIE
 
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import doobie.imports._
val xa = DriverManagerTransactor[Task]("org.h2.Driver", "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "", "")
sql"CREATE SCHEMA IF NOT EXISTS TESTDB".update.run.transact(xa).run
sql"""CREATE TABLE IF NOT EXISTS TESTDB.TEST1(TEST VARCHAR(5) NOT NULL, VALUE DOUBLE NOT NULL, PRIMARY KEY (TEST, VALUE))""".update.run.transact(xa).run
sql"""INSERT INTO TESTDB.TEST1 (TEST, VALUE) VALUES ('test1', 42)""".update.run.transact(xa).run
sql"SELECT TEST, VALUE FROM TESTDB.TEST1".query[(String, Double)].list.transact(xa).run



HTTP4S

import org.http4s.Http4s._
import scalaz.concurrent.Task
import org.http4s.Status.NotFound
import org.http4s.Status.ResponseClass.Redirection
import org.http4s.Status.ResponseClass.Successful

val client = org.http4s.client.blaze.defaultClient
val ret = client(uri("https://www.google.com/")).flatMap {
  case Successful(resp) => resp.as[String].map("Response: " + _)
  case NotFound(resp)   => Task.now("Not Found!")
  case Redirection(resp) => resp.as[String].map("Redirect resp: " + _)
}

ret.run
 
 */
