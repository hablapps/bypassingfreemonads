package org.hablapps.talk

import org.scalatest._

object ChurchApproach extends ChurchApproach

class ChurchApproach extends FlatSpec with Matchers{

  object SingleEffects{    
    // IO Programs

    import scalaz.Monad
    import ObjectAlgebraApproach.IOAlg

    trait IOProgram[T]{
      def apply[P[_]: IOAlg: Monad]: P[T]
    }

    // Particular IO Program
    
    object SingleEffectProgramAdHoc{

      def echo(plus: String) = new IOProgram[String]{
        import IOAlg.Syntax._, scalaz.syntax.monad._

        def apply[P[_]: IOAlg: Monad] = for{
          msg <- read()
          _ <- write(msg + plus)
        } yield msg
      }
    } 

    // IOProgram are IO algebras

    object IOProgram{
      implicit object IOProgramIOAlg extends IOAlg[IOProgram]{
        import IOAlg.Syntax

        def read() = new IOProgram[String]{
          def apply[P[_]: IOAlg: Monad] = 
            Syntax.read[P]()
        }
        def write(msg: String): IOProgram[Unit] = new IOProgram[Unit]{
          def apply[P[_]: IOAlg: Monad] = 
            Syntax.write[P](msg)
        }
      }

      implicit object IOProgramMonad extends Monad[IOProgram]{
        def bind[A,B](p: IOProgram[A])(f: A => IOProgram[B]) = new IOProgram[B]{ 
          def apply[P[_]: IOAlg: Monad] = 
            Monad[P].bind(p.apply[P])(f andThen (_.apply[P]))
        }
        def point[A](a: => A) = new IOProgram[A]{
          def apply[P[_]: IOAlg: Monad] = 
            Monad[P].point(a)
        }
      }
    }

    object SingleEffectProgram{
      import IOAlg.Syntax._, scalaz.syntax.monad._

      def echo(plus: String): IOProgram[String] = for{
        msg <- read[IOProgram]()
        _ <- write[IOProgram](msg + plus)
      } yield msg

    } 

    
    // Echo interpretations
    
    object SingleEffectInterpretations{
      import ObjectAlgebraApproach.{Console, IOStateAction}
      import SingleEffectProgram._
      import scalaz.{Id, Monad}, Id._

      import Console._
      def consoleEcho(plus: String): String = 
        echo(plus)(IOAlg[Id], Monad[Id])

      import IOStateAction._
      def stateEcho(plus: String): IOStateAction[String] = 
        echo(plus)[IOStateAction]
    }

    "State-based echo" should "work" in {
      import ObjectAlgebraApproach.IOState
      import SingleEffectInterpretations._

      stateEcho("").eval(IOState(List("hi"),List())) shouldBe 
        "hi"

      stateEcho("").exec(IOState(List("hi"),List())) shouldBe
        IOState(List(),List("hi"))
    }
  }
  
  object MultipleEffects{

    // IO Programs

    import scalaz.Monad
    import ObjectAlgebraApproach.{LogAlg, IOAlg}

    trait IOProgram[T]{
      def apply[P[_]: IOAlg: LogAlg: Monad]: P[T]
    }
   
    // Generic programs

    import scalaz.Monad
    import IOAlg.Syntax._, LogAlg.Syntax._, scalaz.syntax.monad._
    import ObjectAlgebraApproach.INFO

    def echo(plus: String) = new IOProgram[String]{
      def apply[P[_]: IOAlg: LogAlg: Monad]: P[String] = for{
        msg <- read()
        _ <- log(INFO, s"read '$msg'")
        _ <- write(msg)
        _ <- log(INFO, s"written '$msg'")
      } yield msg
    }

    "IO with logging programs" should "work with io actions" in {
      import ObjectAlgebraApproach.{IOState, IOStateAction}, IOStateAction._
      
      val init: IOState = IOState(List("hi"),List())

      echo("")[IOStateAction].eval(init) shouldBe 
        "hi"

      echo("")[IOStateAction].exec(init) shouldBe 
        IOState(List(), List("INFO: written 'hi'", "hi", "INFO: read 'hi'"))
    }
  }

}