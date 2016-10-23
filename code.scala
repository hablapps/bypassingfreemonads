package org.hablapps.talk

import org.scalatest._

object BypassingFreeMonads extends BypassingFreeMonads

class BypassingFreeMonads extends FlatSpec with Matchers{
  
  object FreeSolution{

    // IO Instructions

    sealed abstract class IOInst[_]
    case class Read() extends IOInst[String]
    case class Write(msg: String) extends IOInst[Unit]
    
    // IO Programs

    import scalaz.Free
    type IO[T] = Free[IOInst,T]

    object IO{
      def read(): IO[String] = Free.liftF(Read())
      def write(msg: String): IO[Unit] = Free.liftF(Write(msg))
    }

    // Particular IO Program
    import IO._, Free._

    def echo(): IO[String] = for{
      msg <- read()
      _ <- write(msg)
    } yield msg

    // Interpretation of IO programs

    import scalaz.~>
    type HK_Algebra[F[_[_],_],P[_]] = F[P,?] ~> P
    type IOAlg2[P[_]] = HK_Algebra[Lambda[(P[_],T)=>IOInst[T]],P]
    type IOAlg[P[_]] = IOInst ~> P

    // Console-based interpretation of IO Programs
    import scalaz.Id, Id.Id

    object ConsoleIO extends IOAlg[Id]{
      import scala.io.StdIn
      def apply[T](inst: IOInst[T]): T = inst match {
        case Read() => StdIn.readLine()
        case Write(msg) => println(msg)
      }
    }

    // State-based interpretation of IO Programs
    import scalaz.State

    case class IOState(in: List[String], out: List[String])

    type IOAction[T]=State[IOState,T]

    object IOAction extends IOAlg[IOAction]{
      import scalaz.syntax.monad._
      
      def apply[T](inst: IOInst[T]): IOAction[T] = inst match {
        case Read() => State.get[IOState] >>= {
          case IOState(msg::reads,written) => 
            State.put(IOState(reads,written)) >| msg
        }
        case Write(msg) => State.get[IOState] >>= {
          case IOState(reads, written) => 
            State.put(IOState(reads, msg::written))
        }
      }
    }

    // Echo interpretations

    def consoleEcho(): String = 
      echo().foldMap(ConsoleIO)

    def stateEcho(): IOAction[String] = 
      echo().foldMap(IOAction)

  }

  "State-based echo" should "work" in {
    import FreeSolution._

    stateEcho().eval(IOState(List("hi"),List())) shouldBe 
      "hi"

    stateEcho().exec(IOState(List("hi"),List())) shouldBe
      IOState(List(),List("hi"))
  }

  object LoggingPrograms{

    // Another set of instructions
    sealed abstract class LoggingInst[_]
    case class Log(level: LoggingInst.Level, msg: String) extends LoggingInst[Unit]

    object LoggingInst{
      sealed abstract class Level
      case object WARNING extends Level
      case object DEBUG extends Level
      case object INFO extends Level
    }

    // Interpretations over IOState 
    import FreeSolution._
    import scalaz.~>
    type LogAlg[P[_]] = LoggingInst ~> P

    object IOAction extends LogAlg[IOAction]{
      def apply[T](inst: LoggingInst[T]): IOAction[T] = inst match {
        case Log(level, msg) => FreeSolution.IOAction(Write(s"$level: $msg"))
      }
    }
  }

  object CoproductSolution{

    // Composing instructions
    import FreeSolution._
    import scalaz.Coproduct
    import LoggingPrograms._

    type IOWithLogInst[T] = Coproduct[IOInst,LoggingInst,T]
  }

  "compound instructions" should "work" in {
    import FreeSolution._
    import CoproductSolution._
    import scalaz.Coproduct
    import LoggingPrograms._, LoggingInst._

    val effect1: IOInst[String] = Read()
    val effect2: LoggingInst[Unit] = Log(INFO,"hi")
    val effect3: IOWithLogInst[String] = Coproduct.left(Read())
    val effect4: IOWithLogInst[Unit] = Coproduct.rightc(Log(INFO,"hi"))

    import scalaz.\/
    Coproduct.right(Log(WARNING,"hi")) shouldBe Coproduct(\/.right(Log(WARNING,"hi")))
  }

  object ProgramsOverCoproducts{

    // Monadic IO programs with logging

    import CoproductSolution._
    import scalaz.Free

    type IOLog[T] = Free[IOWithLogInst,T]

    object IOLog{
      import scalaz.Coproduct, Coproduct._
      import FreeSolution._
      import LoggingPrograms._, LoggingInst._

      def read(): IOLog[String] = 
        Free.liftF[IOWithLogInst,String](Coproduct.left(Read()))
      
      def write(msg: String): IOLog[Unit] = 
        Free.liftF[IOWithLogInst,Unit](Coproduct.left(Write(msg)))
      
      def log(level: Level, msg: String): IOLog[Unit] = 
        Free.liftF[IOWithLogInst,Unit](Coproduct.right(Log(level,msg)))
    }

    // Particular program

    import IOLog._, LoggingPrograms._, LoggingInst._

    def echo(): IOLog[String] = for {
      msg <- read()
      _ <- log(INFO, s"read '$msg'")
      _ <- write(msg)
      _ <- log(INFO, s"written '$msg'")
    } yield msg

    // Interpretation 
    import scalaz.~>

    object IOAction extends (IOWithLogInst ~> FreeSolution.IOAction){
      def apply[T](inst: IOWithLogInst[T]): FreeSolution.IOAction[T] = 
        inst.run.fold(FreeSolution.IOAction(_), LoggingPrograms.IOAction(_))
    }

    def stateEcho(): FreeSolution.IOAction[String] = 
      echo().foldMap(IOAction)
  }


  "IO with logging programs" should "work with io actions" in {
    import ProgramsOverCoproducts._
    import FreeSolution.{stateEcho => _, _}

    val init: IOState = IOState(List("hi"),List())

    stateEcho().eval(init) shouldBe "hi"

    stateEcho().exec(init) shouldBe 
      IOState(List(), List("INFO: written 'hi'", "hi", "INFO: read 'hi'"))
  }














}