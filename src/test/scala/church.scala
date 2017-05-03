package org.hablapps.talk
package bypassingfreemonads

import org.scalatest._

object ChurchApproach extends ChurchApproach

class ChurchApproach extends FlatSpec with Matchers{

  object SingleEffects{    
    // IO Programs

    import scalaz.Monad
    import ObjectAlgebraApproach.IO

    trait IOProgram[T]{
      def apply[P[_]: IO: Monad]: P[T]
    }

    // Particular IO Program
    
    object SingleEffectProgramAdHoc{

      def echo() = new IOProgram[String]{
        import IO.Syntax._, scalaz.syntax.monad._

        def apply[P[_]: IO: Monad] = for{
          msg <- read()
          _ <- write(msg)
        } yield msg
      }
    } 

    // IOProgram are IO algebras

    object IOProgram{
      implicit object IOProgramIO extends IO[IOProgram]{
        import IO.Syntax

        def read() = new IOProgram[String]{
          def apply[P[_]: IO: Monad] = 
            Syntax.read[P]()
        }
        def write(msg: String): IOProgram[Unit] = new IOProgram[Unit]{
          def apply[P[_]: IO: Monad] = 
            Syntax.write[P](msg)
        }
      }

      implicit object IOProgramMonad extends Monad[IOProgram]{
        def bind[A,B](p: IOProgram[A])(f: A => IOProgram[B]) = new IOProgram[B]{ 
          def apply[P[_]: IO: Monad] = 
            Monad[P].bind(p.apply[P])(f andThen (_.apply[P]))
        }
        def point[A](a: => A) = new IOProgram[A]{
          def apply[P[_]: IO: Monad] = 
            Monad[P].point(a)
        }
      }
    }

    object SingleEffectProgram{
      import IO.Syntax._, scalaz.syntax.monad._

      def echo(): IOProgram[String] = for{
        msg <- read[IOProgram]()
        _ <- write[IOProgram](msg)
      } yield msg

    } 

    
    // Echo interpretations
    
    object SingleEffectInterpretations{
      import ObjectAlgebraApproach.{Console, IOStateAction}
      import SingleEffectProgram._
      import scalaz.{Id, Monad}, Id._

      import Console._
      def consoleEcho(): String = 
        echo()(IO[Id], Monad[Id])

      import IOStateAction._
      def stateEcho(): IOStateAction[String] = 
        echo()[IOStateAction]
    }

    "State-based echo" should "work" in {
      import ObjectAlgebraApproach.IOState
      import SingleEffectInterpretations._

      stateEcho().eval(IOState(List("hi"),List())) shouldBe 
        "hi"

      stateEcho().exec(IOState(List("hi"),List())) shouldBe
        IOState(List(),List("hi"))
    }
  }
  
  object MultipleEffects{

    // IO Programs

    import scalaz.Monad
    import ObjectAlgebraApproach.{Log, IO}

    trait IOProgram[T]{
      def apply[P[_]: IO: Log: Monad]: P[T]
    }
   
    // Generic programs

    import scalaz.Monad
    import IO.Syntax._, Log.Syntax._, scalaz.syntax.monad._
    import ObjectAlgebraApproach.INFO

    def echo() = new IOProgram[String]{
      def apply[P[_]: IO: Log: Monad]: P[String] = for{
        msg <- read()
        _ <- log(INFO, s"read '$msg'")
        _ <- write(msg)
        _ <- log(INFO, s"written '$msg'")
      } yield msg
    }

    "IO with logging programs" should "work with io actions" in {
      import ObjectAlgebraApproach.{IOState, IOStateAction}, IOStateAction._
      
      val init: IOState = IOState(List("hi"),List())

      echo()[IOStateAction].eval(init) shouldBe 
        "hi"

      echo()[IOStateAction].exec(init) shouldBe 
        IOState(List(), List("INFO: written 'hi'", "hi", "INFO: read 'hi'"))
    }
  }

  object DefiningChurch{
    import scalaz.Monad
    import ObjectAlgebraApproach.IO, IO.Syntax._, scalaz.syntax.monad._

    trait Church[Alg[_[_]], T]{
      def apply[P[_]: Alg: Monad]: P[T]
    }

    type IOProgram[T] = Church[IO,T]

    def echo[P[_]: IO: Monad](): P[String] = ???

    def echo(): { def apply[P[_]: IO: Monad]: P[String] } = ??? 

    def echo2(): Church[IO,String] = new IOProgram[String]{
      def apply[P[_]: IO: Monad] = for{
        msg <- read()
        _ <- write(msg)
      } yield msg
    }

  }

}