package parser

import java.io.FileReader
import bnet.Bnet
import base.VariableSet

import scala.collection.mutable.ListBuffer

/**
 * Object offering a main method for making the parser
 * of a net. It is just a class for testing parsing
 * functionality
 */
object BnetParserMain extends BnetParser {
   def main(args: Array[String]): Unit = {
      val lector = new FileReader(args(0))
      val result: BnetParserMain.ParseResult[Bnet] = parseAll(component, lector)
      val net: Bnet = result.get
      val analysisResults: List[(VariableSet, List[Double], Double,
                           List[Double], List[Double], Double, Long, Long)] =
         Bnet.analyzePotentials(net)

      // shows the result of the analysis
      analysisResults.foreach(x => {
         println("potential.Potential for variables----------------------")
         println(x._1)
         println("----------- potential num values ------------")
         println(x._3)
         println("----------- set of values -------------------")
         println(println(x._2))
         println("------------ real num values --------------")
         println("Number of different values: " + x._4.size)
         println("Values: -----------------------------------")
         println(x._4)
         println("--------- proportions ------------")
         println(x._5)
         println()
         println("----------cero proportions -------------")
         println(x._6)
         println()
         println()
      })

      println("-----------------------------------------------------------")
      println("Summary of results ----------------------------------------")
      println("bnet.Variable   pot. size   values   prop values / pot. size   min prop.    max.prop    cero prop.")
      analysisResults.foreach(x => {
         print(x._1.getVariable(x._1.getSize - 1).name)
         printf("%12.0f", x._3)
         printf("%12d", x._4.size)
         printf("%12.3f", x._4.size * 100 / x._3)
         printf("%12.3f", x._5.min)
         printf("%12.3f", x._5.max)
         printf("%12.3f", x._6)
         println()
      })
      val potentialSizes: List[Double] = analysisResults.map(x => x._3)
      val meanSize = potentialSizes.sum / potentialSizes.size.toDouble
      println("potential sizes:  mean    max   min")
      printf("%12.3f", meanSize)
      printf("%12.3f", potentialSizes.max)
      printf("%12.3f", potentialSizes.min)
      println()

      val proportions: List[Double] = analysisResults.map(x => x._4.size * 100 / x._3)
      val meanProportion = proportions.sum / proportions.size
      println("proportion possible values / different values:  mean    max   min")
      printf("%12.3f", meanProportion)
      printf("%12.3f", proportions.max)
      printf("%12.3f", proportions.min)
      println()
      println("-----------------------------------------------------------")

      val cerosInfo: List[Double] = analysisResults.map(x => x._6)
      println("Average ceros prop.      max ceros prop.     min ceros prop.")
      printf("%12.3f", cerosInfo.sum / cerosInfo.length)
      printf("%12.3f", cerosInfo.max)
      printf("%12.3f", cerosInfo.min)
      println()
      println("-----------------------------------------------------------")
   }
}
