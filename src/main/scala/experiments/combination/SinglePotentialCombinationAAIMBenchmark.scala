package experiments.combination

import org.scalameter.{Key, Warmer, config}
import potential.{OperatorType, Potential, ValueStoreTypes}

/**
 * class for testing combination on a particular pair
 * of potentials
 */
object SinglePotentialCombinationAAIMBenchmark extends App{
   /**
    * parameters defining the folder where the potentials
    * are stored
    */
   val maxCard = 180000
   val minCard = 90000
   val levels = 50
   val id = 0

   //val levels = List(10, 50, 100, 500, 1000, 5000, 10000)

   // compose the path to the folder containing the objects
   // with potential values
   val folder = "./data/potentialObjects/comb/" + maxCard + "-" +
      minCard + "-" + levels + "/"

   // read potential objects
   val filename1 = folder + "pot1-" + id + ".pot"
   val filename2 = folder + "pot2-" + id + ".pot"
   println("    potential 1 name: " + filename1)
   println("    potential 2 name: " + filename2)

   // defines the configuration for benchmarking
   val standardConfig = config(
      Key.exec.minWarmupRuns -> 20,
      Key.exec.maxWarmupRuns -> 40,
      Key.exec.benchRuns -> 30,
      Key.verbose -> false
   ) withWarmer new Warmer.Default

   // read potential and indexes
   val potential1 = Potential.readObject(filename1)
   val potential2 = Potential.readObject(filename2)
   println("pot 1 cardinality: " + potential1.variables.possibleValues)
   println("pot 2 cardinality: " + potential2.variables.possibleValues)

   // perform combination with tables representation
   potential1.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)

   // compute combination time for tables
   val timeTable = (standardConfig measure {
      potential1.combine(potential2)
   }).value
   println("Time table: " + timeTable)

   // now convert potentials into trees and compare the result
   val pot1Tree = potential1.convert(ValueStoreTypes.TREE)
   val pot2Tree = potential2.convert(ValueStoreTypes.TREE)
   pot1Tree.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)

   // compute combination time for trees
   val timeTree = (standardConfig measure {
      pot1Tree.combine(pot2Tree)
   }).value
   println("Time tree: " + timeTree)

   // now convert to AAIM store
   val pot1AAIM = potential1.convert(ValueStoreTypes.IDPMSTORE)
   val pot2AAIM = potential2.convert(ValueStoreTypes.IDPMSTORE)

   // set functions to def - def
   pot1AAIM.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)

   // compute time
   var timeAAIM = (standardConfig measure {
      pot1AAIM.combine(pot2AAIM)
   })
   println("Time AAIM def - def: " + timeAAIM)

   // set functions to Alt1 - def
   /*pot1AAIM.setFunctions(OperatorType.ALT1, OperatorType.DEFAULT)

   // compute time
   timeAAIM = (standardConfig measure {
      pot1AAIM.combine(pot2AAIM)
   })
   println("Time AAIM ALT1 - def: " + timeAAIM)

   // ALT2 for combination ----------------------------------
   // set functions to Alt2 - def
   pot1AAIM.setFunctions(OperatorType.ALT2, OperatorType.DEFAULT)

   // compute time
   timeAAIM = (standardConfig measure {
      pot1AAIM.combine(pot2AAIM)
   })
   println("Time AAIM ALT2 - def: " + timeAAIM)

   // ALT3 for combination ----------------------------------
   // set functions to Alt3 - def
   pot1AAIM.setFunctions(OperatorType.ALT3, OperatorType.DEFAULT)

   // compute time
   timeAAIM = (standardConfig measure {
      pot1AAIM.combine(pot2AAIM)
   })
   println("Time AAIM ALT3 - def: " + timeAAIM)

   // ALT4 for combination ----------------------------------
   // set functions to Alt4 - def
   pot1AAIM.setFunctions(OperatorType.ALT4, OperatorType.DEFAULT)

   // compute time
   timeAAIM = (standardConfig measure {
      pot1AAIM.combine(pot2AAIM)
   })
   println("Time AAIM ALT4 - def: " + timeAAIM)

   // ALT5 for combination ----------------------------------
   // set functions to Alt5 - def
   pot1AAIM.setFunctions(OperatorType.ALT5, OperatorType.DEFAULT)

   // compute time
   timeAAIM = (standardConfig measure {
      pot1AAIM.combine(pot2AAIM)
   })
   println("Time AAIM ALT5 - def: " + timeAAIM)

   // ALT6 for combination ----------------------------------
   // set functions to Alt6 - def
   pot1AAIM.setFunctions(OperatorType.ALT6, OperatorType.DEFAULT)

   // compute time
   timeAAIM = (standardConfig measure {
      pot1AAIM.combine(pot2AAIM)
   })
   println("Time AAIM ALT6 - def: " + timeAAIM)

   // ALT7 for combination ----------------------------------
   // set functions to Alt7 - def
   pot1AAIM.setFunctions(OperatorType.ALT7, OperatorType.DEFAULT)

   // compute time
   timeAAIM = (standardConfig measure {
      pot1AAIM.combine(pot2AAIM)
   })
   println("Time AAIM ALT7 - def: " + timeAAIM)
  */
}
