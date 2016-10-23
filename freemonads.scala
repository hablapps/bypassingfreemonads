package org.hablapps.talk

import org.scalatest._

object FreeMonadApproach extends FreeMonadApproach

class FreeMonadApproach extends FlatSpec with Matchers{
    
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
  
  object SingleEffectProgram{
    import IO._, Free._

    def echo(): IO[String] = for{
      msg <- read()
      _ <- write(msg)
    } yield msg
  } 

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

  type IOStateAction[T]=State[IOState,T]

  object IOStateActionIO extends IOAlg[IOStateAction]{
    import scalaz.syntax.monad._
    
    def apply[T](inst: IOInst[T]): IOStateAction[T] = inst match {
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
  
  object SingleEffectInterpretations{
    import SingleEffectProgram._

    def consoleEcho(): String = 
      echo().foldMap(ConsoleIO)

    def stateEcho(): IOStateAction[String] = 
      echo().foldMap(IOStateActionIO)
  }

  "State-based echo" should "work" in {
    import SingleEffectInterpretations._

    stateEcho().eval(IOState(List("hi"),List())) shouldBe 
      "hi"

    stateEcho().exec(IOState(List("hi"),List())) shouldBe
      IOState(List(),List("hi"))
  }


  // Logging instructions
  
  sealed abstract class LogInst[_]
  case class Log(level: Level, msg: String) extends LogInst[Unit]

  sealed abstract class Level
  case object WARNING extends Level
  case object DEBUG extends Level
  case object INFO extends Level

  // Interpretations over IOState 
  
  type LogAlg[P[_]] = LogInst ~> P

  object IOStateActionLog extends LogAlg[IOStateAction]{
    def apply[T](inst: LogInst[T]): IOStateAction[T] = inst match {
      case Log(level, msg) => IOStateActionIO(Write(s"$level: $msg"))
    }
  }

  // Composing instructions

  import scalaz.Coproduct

  type IOWithLogInst[T] = Coproduct[IOInst,LogInst,T]

  "compound instructions" should "work" in {
    import LogInst._

    val effect1: IOInst[String] = Read()
    val effect2: LogInst[Unit] = Log(INFO,"hi")
    val effect3: IOWithLogInst[String] = Coproduct.left(Read())
    val effect4: IOWithLogInst[Unit] = Coproduct.rightc(Log(INFO,"hi"))

    import scalaz.\/
    Coproduct.right(Log(WARNING,"hi")) shouldBe Coproduct(\/.right(Log(WARNING,"hi")))
  }

  // Monadic IO programs with logging

  type IOLog[T] = Free[IOWithLogInst,T]

  object IOLog{
    import Coproduct._
    
    def read(): IOLog[String] = 
      Free.liftF[IOWithLogInst,String](left(Read()))
    
    def write(msg: String): IOLog[Unit] = 
      Free.liftF[IOWithLogInst,Unit](left(Write(msg)))
    
    def log(level: Level, msg: String): IOLog[Unit] = 
      Free.liftF[IOWithLogInst,Unit](right(Log(level,msg)))
  }

  // Particular program
  
  object MultipleEffectProgram{
    import IOLog._

    def echo(): IOLog[String] = for {
      msg <- read()
      _ <- log(INFO, s"read '$msg'")
      _ <- write(msg)
      _ <- log(INFO, s"written '$msg'")
    } yield msg
  }

  // Interpretation 
  
  object IOStateActionIOLog extends (IOWithLogInst ~> IOStateAction){
    def apply[T](inst: IOWithLogInst[T]): IOStateAction[T] = 
      inst.run.fold(IOStateActionIO(_), IOStateActionLog(_))
  }

  object MultipleEffectInterpretation{
    import MultipleEffectProgram._

    def stateEcho(): IOStateAction[String] = 
      echo().foldMap(IOStateActionIOLog)
  }

  "IO with logging programs" should "work with io actions" in {
    import MultipleEffectInterpretation._
    
    val init: IOState = IOState(List("hi"),List())

    stateEcho().eval(init) shouldBe "hi"

    stateEcho().exec(init) shouldBe 
      IOState(List(), List("INFO: written 'hi'", "hi", "INFO: read 'hi'"))
  }

  // Smart constructors for IO instructions prepared for Coproducts
  
  import scalaz.Inject

  object IOInst{

    def read[I[_]]()(implicit I: Inject[IOInst,I]): Free[I,String] = 
      Free.liftF(I.inj(Read()))

    def write[I[_]](msg: String)(implicit I: Inject[IOInst,I]): Free[I,Unit] = 
      Free.liftF(I.inj(Write(msg)))
  }

  object LogInst{
    def log[I[_]](level: Level, msg: String)(implicit I: Inject[LogInst,I]): Free[I,Unit] = 
      Free.liftF(I.inj(Log(level,msg)))
  }

  // Generic programs

  object MultipleEffectAbstractProgram{
    import IOInst._, LogInst._

    def echo[I[_]: Inject[IOInst,?[_]]: Inject[LogInst,?[_]]](): Free[I,String] = for {
      msg <- read()
      _ <- log(INFO, s"read '$msg'")
      _ <- write(msg)
      _ <- log(INFO, s"written '$msg'")
    } yield msg
  }

  object MultipleEffectAbstractInterpretation{
    import MultipleEffectAbstractProgram._
    import scalaz.Inject._

    def consoleEcho(): IOStateAction[String] = 
      echo[IOWithLogInst]().foldMap(IOStateActionIOLog)
  }

  

}