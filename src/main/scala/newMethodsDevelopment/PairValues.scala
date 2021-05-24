package newMethodsDevelopment

/**
 * Checks ways of making pairs of values and grouping
 * them according to the result
 */
object PairValues extends App{
   val c1 = List(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
   val c2 = List(0.2, 0.3, 0.4)

   val triplets: List[(Double, Double, Double)] = for(val1 <- c1; val2 <- c2)
         yield (val1 * val2, val1, val2)

   val grouped = triplets.groupBy(_._1)

   println(grouped.mkString("\n"))
}

