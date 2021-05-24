package utils

import java.io.File
import scala.annotation.tailrec

/**
 * Object for utility methods
 */
object Util{
   /**
    * Constants of general use
    */
   val DIF : Double = 0.001
   val DECIMALS = 4

   /**
    * Constant for default value
    */
   val DEFAULTVALUE = 0.0

   /**
    * Compares two values testing for equality for real numbers
    * @param x first value
    * @param y second value
    * @return result of the check
    */
   def nearEqual(x: Double, y : Double) : Boolean = {
      if(Math.abs(x - y) < DIF) true
      else false
   }

   /**
    * round a number to a given number of decimals
    * @param x value to round
    * @return result of operation
    */
   def roundNumber(x : Double) : Double = {
      try {
         BigDecimal(x).
            setScale(DECIMALS, BigDecimal.RoundingMode.HALF_UP).
            doubleValue()
      }
      catch{
         case _: Exception =>
            println("Exception converting " + x + "to BigDecimal")
            System.exit(0)
            0.0
      }
   }

   /**
    * Check equality to 0
    * @param x value to check
    * @return result of check
    */
   def nearZero(x : Double) : Boolean = {
      Math.abs(x - 0.0) < DIF
   }

   /**
    * reduce similar values using roundNumber
    * @param valueList list of values to analyze
    * @return list of values after processing
    */
   def reduceSimilarValues(valueList : List[Double]) : List[Double] = {
      // auxiliary method
      @tailrec
      def go(toConsider : List[Double], stored : List[Double]) :
      List[Double] = {
         toConsider match{
            case first::rest =>
               // check if some similar value is already stored
               val similar=stored.
                  filter(value => Math.abs(first-value) < DIF)
               if(similar.isEmpty) {
                  go(rest, first::stored)
               }
               else {
                  // keep on processing if needed
                  go(rest, stored)
               }
            case Nil => stored
         }
      }

      // just call go auxiliary method
      go(valueList.map(value => roundNumber(value)), List())
   }

   /**
    * Determine the default value
    * @return default value obtained
    */
   def determineDefaultValue(values : Seq[Double]) : Double = {
      // computes the element with more occurrences
      val freqMap: (Int, Double) =
         values.groupBy(identity).
            map(entry =>(entry._2.size,entry._1)).maxBy(_._1)

      // return the value (second part of freqMap)
      freqMap._2
   }

   /**
    * Gets the number of occurrences of a given value
    * @param refValue reference value
    * @param values target values
    * @return
    */
   def getOccurrencesCounter(refValue : Double,
                             values : Seq[Double]): Int = {
      values.count(_ == refValue)
   }

   /**
    * Gets the list of files with a given extension
    * @param dir folder name
    * @param ext extension of target files
    * @return
    */
   def getListOfFiles(dir: String, ext : String):List[File] = {
      val d = new File(dir)
      if (d.exists && d.isDirectory) {
         d.listFiles.filter(_.isFile).
            toList.filter(_.getName.endsWith(ext))
      } else {
         List[File]()
      }
   }

   /**
    * makes triples from two lists of values. The content of triplet is
    * a) product of val1*val2
    * b) val1 (coming from list1)
    * v( val2 (coming from list2)
    * @param list1 first list of values
    * @param list2 second list of values
    * @return map with products as keys and triplets as values. Values are
    *         lists because the same product can be obtained with different
    *         pairs of values
    */
   def makeTriplets(list1 : List[Double], list2 : List[Double]) :
                                          Map[Double,List[(Double, Double, Double)]] = {
      val triplets = for(val1 <- list1; val2 <- list2)
         yield (Util.roundNumber(val1 * val2), val1, val2)
      triplets.groupBy(_._1)
   }

   /**
    * produces all the pairs combining elements from list1 and list2
    * @param list1 first list of indexes
    * @param list2 second list of indexes
    * @return list of tuples
    */
   def makeCombinations(list1 : List[Long], list2 : List[Long]) : List[(Long, Long)] = {
      for(index1 <- list1; index2 <- list2)
         yield (index1, index2)
   }

   /**
    * computes the mean of a sequence of values
    * @param a sequence of values
    * @return
    */
   def mean(a: Seq[Double]): Double = a.sum / a.length

   /**
    * computes the standard deviation of a sequence of values
    * @param data sequence of values to analyze
    * @return
    */
   def std(data: Seq[Double]): Double = {
      val avg = mean(data)
      math.sqrt(data.map(x => math.pow(x - avg,2)).sum / data.length)
   }
}

