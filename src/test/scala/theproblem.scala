package org.hablapps.talk
package bypassingfreemonads

import scala.io.StdIn.readLine
import org.scalatest._

object ConventionalApproach extends ConventionalApproach

class ConventionalApproach extends FlatSpec with Matchers{

  object NonModular{
  
    def echo(): String = {
      val msg: String = readLine()
      println(msg)
      msg
    }
  }

  object Modular{

    trait IO{
      def read(): String 
      def write(msg: String): Unit
    }

    def echo()(io: IO): String = {
      val msg: String = io.read()
      io.write(msg)
      msg
    }

    object ConsoleIO extends IO{
      def read() = readLine
      def write(msg: String) = println(msg)
    }

    def consoleEcho(): String = 
      echo()(ConsoleIO)

  }

  object ModularWithMultipleAPIs{

    trait Log{
      def log(level: Log.Level, msg: String): Unit 
      def warn(msg: String): Unit = log(Log.WARN,msg)
      def debug(msg: String): Unit = log(Log.DEBUG,msg)
      def trace(msg: String): Unit = log(Log.TRACE,msg)
    }

    object Log{
      sealed abstract class Level
      case object WARN extends Level
      case object DEBUG extends Level
      case object TRACE extends Level
    }

    object ConsoleLog extends Log{
      def log(level: Log.Level, msg: String) = 
        println(s"$level: $msg")
    }

    import Modular.IO

    def echoWithLog()(io: IO, log: Log): String = {
      val msg: String = io.read()
      log.trace(s"read $msg")
      io.write(msg)
      log.trace(s"written $msg")
      msg 
    }

    import Modular.ConsoleIO
    def consoleEchoWithLog(): String = 
      echoWithLog()(ConsoleIO, ConsoleLog)
  }

  object ButNotGeneral{
    import scala.concurrent.{Await, Future, ExecutionContext, duration}
    import ExecutionContext.Implicits.global, duration._

    object WrongAsyncIO extends Modular.IO{

      def read(): String = {
        val futureMsg: Future[String] = ???
        Await.result(futureMsg, 1 second)
      }
      def write(msg: String): Unit = ???
    }

    trait IO{
      def read(): Future[String]
      def write(msg: String): Future[Unit]
    }

    object AsyncIO extends IO{
      def read(): Future[String] = Future(scala.io.StdIn.readLine())
      def write(msg: String): Future[Unit] = Future(println(msg))
    }

    def echo()(io: IO): Future[String] =
      io.read().flatMap{ msg => 
        io.write(msg).flatMap{ _ => 
          Future.successful(msg)
        }
      }
  }

  object DSLsToTheRescue{

    // (Ad-hoc) DSL

    sealed abstract class IOProgram[_]
    case class Read() extends IOProgram[String]
    case class Write(msg: String) extends IOProgram[Unit]
    case class FlatMap[A,B](p: IOProgram[A], f: A => IOProgram[B]) extends IOProgram[B]
    case class Returns[A](a: A) extends IOProgram[A]

    // TRULY GENERAL PROGRAMS 

    def echo(): IOProgram[String] =
      FlatMap(Read(), { msg: String => 
        FlatMap(Write(msg), { _: Unit => 
          Returns(msg)
        })
      })

    // INTERPRETERS

    import scalaz.~>
    import scalaz.Id, Id._, scalaz.syntax.id._
    
    type IOInterpreter[P[_]] = IOProgram ~> P

    object ConsoleIO extends IOInterpreter[Id]{
      def apply[T](program: IOProgram[T]) = program match {
        case Read() => readLine()
        case Write(msg) => println(msg)
        case Returns(a) => a
        case FlatMap(p, f) => ConsoleIO(p) |> f |> ConsoleIO.apply
      }
    }

    // COMPOSITION

    def consoleEcho(): String = ConsoleIO(echo())

  }

  object ProblemsOfAdHocDSLs{

    // As we can see, in order to create a new imperative DSL we have 
    // to copy & paste the cases that allow us to write imperative programs.
    object NewImperativeDSL{

      sealed abstract class FileSystemProgram[_]
      case class ReadFile(path: String) extends FileSystemProgram[String]
      case class WriteFile(path: String, contents: String) extends FileSystemProgram[Unit]
      case class DeleteFile(path: String) extends FileSystemProgram[Unit]
      case class FlatMap[A,B](p: FileSystemProgram[A], f: A => FileSystemProgram[B]) extends FileSystemProgram[B]
      case class Returns[A](a: A) extends FileSystemProgram[A]
    }

    // Moreover, what if we need to write imperative programs that refer both
    // to IO and FileSystem instructions?
    object MultipleEffects{
      
      sealed abstract class FileSystemIOProgram[_]
      case class Read() extends FileSystemIOProgram[String]
      case class Write(msg: String) extends FileSystemIOProgram[Unit]
      case class ReadFile(path: String) extends FileSystemIOProgram[String]
      case class WriteFile(path: String, contents: String) extends FileSystemIOProgram[Unit]
      case class DeleteFile(path: String) extends FileSystemIOProgram[Unit]
      case class FlatMap[A,B](p: FileSystemIOProgram[A], f: A => FileSystemIOProgram[B]) extends FileSystemIOProgram[B]
      case class Returns[A](a: A) extends FileSystemIOProgram[A]
    }
  }

}