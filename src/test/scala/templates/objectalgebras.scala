package org.hablapps.talk
package bypassingfreemonads
package templates

import scala.io.StdIn.readLine
import org.scalatest._

/**
To exercise this live coding type this in the sbt prompt: 

> ~testOnly org.hablapps.talk.bypassingfreemonads.templates.ObjectAlgebraApproach

*/
object ObjectAlgebraApproach extends ObjectAlgebraApproach

class ObjectAlgebraApproach extends FlatSpec with Matchers{

  object SingleEffects{

    trait IO{
      def read(): String 
      def write(msg: String): Unit
    }

    // def echo()(io: IO): String = {
    //   val msg: String = io.read()
    //   io.write(msg)
    //   msg
    // }

    // Synchronous API

    // object ConsoleIO extends IO{
    //   def read() = readLine
    //   def write(msg: String) = println(msg)
    // }

    // def consoleEcho(): String = 
    //   echo()(ConsoleIO)

    // Asynchronous API

    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global


  }

  "Single effects" should "work" in {
    import SingleEffects._

  } 

  object MultipleEffects{

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

    // object ConsoleLog extends Log{
    //   def log(level: Log.Level, msg: String) = 
    //     println(s"$level: $msg")
    // }

    // import SingleEffects.IO

    // def echoWithLog()(io: IO, log: Log): String = {
    //   val msg: String = io.read()
    //   log.trace(s"read $msg")
    //   io.write(msg)
    //   log.trace(s"written $msg")
    //   msg 
    // }

    // import SingleEffects.ConsoleIO
    // def consoleEchoWithLog(): String = 
    //   echoWithLog()(ConsoleIO, ConsoleLog)
  }

}