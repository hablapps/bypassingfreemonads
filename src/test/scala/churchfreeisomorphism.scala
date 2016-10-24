package org.hablapps.talk

import org.scalatest._

object ChurchFreeIsomorphism extends ChurchFreeIsomorphism

class ChurchFreeIsomorphism extends FlatSpec with Matchers{

  import ChurchApproach.{SingleEffects => C}
  val FM = FreeMonadApproach
  val OA = ObjectAlgebraApproach

  object ChurchIOAlg extends FM.IOAlg[C.IOProgram]{
    def apply[T](inst: FM.IOInst[T]) = inst match {
      case FM.Read() => C.IOProgram.IOProgramIOAlg.read()
      case FM.Write(msg) => C.IOProgram.IOProgramIOAlg.write(msg)
    }
  }

  object FreeIOProgramIOAlg extends OA.IOAlg[FM.IOProgram]{
    def read(): FM.IOProgram[String] = 
      FM.IOProgram.Syntax.read()
    def write(msg: String): FM.IOProgram[Unit] = 
      FM.IOProgram.Syntax.write(msg)
  }

  import scalaz.Isomorphism._

  val Iso: C.IOProgram <~> FM.IOProgram = 
    new IsoFunctorTemplate[C.IOProgram, FM.IOProgram]{
      def to[T](ioprogram: C.IOProgram[T]): FM.IOProgram[T] = 
        ioprogram(FreeIOProgramIOAlg, scalaz.Monad[FM.IOProgram])

      def from[T](ioprogram: FM.IOProgram[T]): C.IOProgram[T] = 
        ioprogram.foldMap(ChurchIOAlg)
    }

}