package parser

import java.io.FileReader
import base.VariableSet
import bnet.Bnet

import scala.collection.mutable.ListBuffer

/**
 * Object offering a min method for testing the functionality
 * of parsing UAI networks
 */
object UAINetParserMain extends UAINetParser {
   def main(args: Array[String]): Unit = {
      // creates the reader
      val lector = new FileReader(args(0))
      print("parsing UAI file: " + args(0))

      // get parser result
      //val result: UAINetParserMain.ParseResult[Bnet] =
      val net = parse(component, lector) match {
         case Success(result, _) =>
            print("parse success")
            result
         case Failure(msg, _) =>
            println("Failure: " + msg)
            null
         case Error(msg, _) =>
            println("Error: " + msg)
            null
      }

      // perform the analysis of the net
      val analysisResults: List[(VariableSet, List[Double], Double,
         List[Double], List[Double], Double, Long, Long)] =
            Bnet.analyzePotentials(net)
   }
}
