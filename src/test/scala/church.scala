package org.hablapps.talk

import org.scalatest._

object ChurchApproach extends ChurchApproach

class ChurchApproach extends FlatSpec with Matchers{
    
  // IO Programs

  trait IOProgram[T]{
    def apply[P[_]](ioalg: IOAlg[P], monad: Monad[P]): P[T]
  }

  // Particular IO Program
  
  object SingleEffectProgram{
    import scalaz.Monad
    import IOAlg.Syntax._, scalaz.syntax.monad._

    def echo[P[_]: IOAlg: Monad](): P[String] = for{
      msg <- read()
      _ <- write(msg)
    } yield msg
  } 

  // Console-based interpretation of IO Programs

  import scalaz.Id, Id.Id

  object Console{
    implicit object ConsoleIO extends IOAlg[Id]{
      def read() = scala.io.StdIn.readLine()
      def write(msg: String) = println(msg)
    }
  }

  // State-based interpretation of IO Programs

  import scalaz.State

  case class IOState(in: List[String], out: List[String])

  type IOStateAction[T]=State[IOState,T]

  object IOStateAction{
    implicit object IOStateActionIO extends IOAlg[IOStateAction]{
      import scalaz.syntax.monad._
      
      def read() = State.get[IOState] >>= {
        case IOState(msg::reads,written) => 
          State.put(IOState(reads,written)) >| msg
      }

      def write(msg: String) = State.get[IOState] >>= {
        case IOState(reads, written) => 
          State.put(IOState(reads, msg::written))
      }
    }
  }

  // Echo interpretations
  
  object SingleEffectInterpretations{
    import SingleEffectProgram._
    import scalaz.Monad

    def consoleEcho(): String = 
      echo()(Console.ConsoleIO, Monad[Id])

    import IOStateAction._
    def stateEcho(): IOStateAction[String] = 
      echo[IOStateAction]()
  }

  "State-based echo" should "work" in {
    import SingleEffectInterpretations._

    stateEcho().eval(IOState(List("hi"),List())) shouldBe 
      "hi"

    stateEcho().exec(IOState(List("hi"),List())) shouldBe
      IOState(List(),List("hi"))
  }

  // Interpretations over IOState 
  
  sealed abstract class Level
  case object WARNING extends Level
  case object DEBUG extends Level
  case object INFO extends Level
  
  trait LogAlg[P[_]]{
    def log(level: Level, msg: String): P[Unit]
  }

  object LogAlg{
    object Syntax{
      def log[P[_]](level: Level, msg: String)(implicit Log: LogAlg[P]) = 
        Log.log(level,msg)
    }

    implicit object IOStateActionLog extends LogAlg[IOStateAction]{
      def log(level: Level, msg: String) = 
        IOStateAction.IOStateActionIO.write(s"$level: $msg")
    }
  }

  // Generic programs

  object MultipleEffectAbstractProgram{
    import scalaz.Monad
    import IOAlg.Syntax._, LogAlg.Syntax._, scalaz.syntax.monad._

    def echo[P[_]: IOAlg: LogAlg: Monad](): P[String] = for {
      msg <- read()
      _ <- log(INFO, s"read '$msg'")
      _ <- write(msg)
      _ <- log(INFO, s"written '$msg'")
    } yield msg
  }

  "IO with logging programs" should "work with io actions" in {
    import MultipleEffectAbstractProgram._
    import IOStateAction._
    
    val init: IOState = IOState(List("hi"),List())

    echo[IOStateAction]().eval(init) shouldBe 
      "hi"

    echo[IOStateAction]().exec(init) shouldBe 
      IOState(List(), List("INFO: written 'hi'", "hi", "INFO: read 'hi'"))
  }

}