package benchmarkTest.operationsEvaluation

import bnet.Bnet
import inference.VariableElimination
import org.scalameter._
import potential.{OperatorType, ValueStoreTypes}

/**
  * Object to perform comparisons in the application of
  * VariableElimination algorithm on a given network and
  * ASIM using different alternatives for combination
  * and marginalization operations
  */
object VETableTreeASIMBenchmark extends App {

   // defines the configuration for benchmarking
   val standardConfig = config(
      Key.exec.minWarmupRuns -> 20,
      Key.exec.maxWarmupRuns -> 40,
      Key.exec.benchRuns -> 100,
      Key.verbose -> false
   ) withWarmer new Warmer.Default

   // define the net name to propagate
   var netName = "asia"
   var extension="net"

   // creates the engine for tables
   val bnet = Bnet(netName + "." + extension)
   val engineTables=new VariableElimination(bnet,false)

   // creates a engine for tree
   val bnetTree = Bnet.convert(bnet, ValueStoreTypes.TREE)
   val engineTree = new VariableElimination(bnetTree, false)

   // creates a engine for Map indices
   val bnetMapIndices = Bnet.convert(bnet, ValueStoreTypes.IDSMSTORE)
   val engineASIMutable = new VariableElimination(bnetMapIndices, false)

   // general test: propagation with CPT, PTs and AIMut
   // starts with CPT representation
   val cptTime = standardConfig measure {
      engineTables.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
      engineTables.propagate
   }

   val treeTime = standardConfig measure {
      engineTree.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
      engineTree.propagate
   }

   println("...............starting work with ASIIM - def - def ..............")
   val aimutTimeDefDef = standardConfig measure {
      engineASIMutable.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
      engineASIMutable.propagate
   }

   println("...............starting work with ASIIM - alt1 - def ..............")
   val aimutTimeAlt1Def = standardConfig measure {
      engineASIMutable.setFunctions(OperatorType.ALT1, OperatorType.DEFAULT)
      engineASIMutable.propagate
   }

   println("...............starting work with ASIIM - alt1 - alt1 ..............")
   val aimutTimeAlt1Alt1 = standardConfig measure {
      engineASIMutable.setFunctions(OperatorType.ALT1, OperatorType.ALT1)
      engineASIMutable.propagate
   }

   println("...............starting work with ASIIM - alt1 - alt2 ..............")
   val aimutTimeAlt1Alt2 = standardConfig measure {
      engineASIMutable.setFunctions(OperatorType.ALT1, OperatorType.ALT2)
      engineASIMutable.propagate
   }

   println("...............starting work with ASIIM - alt1 - alt3 ..............")
   val aimutTimeAlt1Alt3 = standardConfig measure {
      engineASIMutable.setFunctions(OperatorType.ALT1, OperatorType.ALT2)
      engineASIMutable.propagate
   }

   println("...............starting work with ASIIM - alt2 - def ..............")
   val aimutTimeAlt2Def = standardConfig measure {
      engineASIMutable.setFunctions(OperatorType.ALT2, OperatorType.DEFAULT)
      engineASIMutable.propagate
   }

   println("...............starting work with ASIIM - alt2 - alt1 ..............")
   val aimutTimeAlt2Alt1 = standardConfig measure {
      engineASIMutable.setFunctions(OperatorType.ALT2, OperatorType.ALT1)
      engineASIMutable.propagate
   }

   println("...............starting work with ASIIM - alt2 - alt2 ..............")
   val aimutTimeAlt2Alt2 = standardConfig measure {
      engineASIMutable.setFunctions(OperatorType.ALT2, OperatorType.ALT2)
      engineASIMutable.propagate
   }

   println("...............starting work with ASIIM - alt2 - alt3 ..............")
   val aimutTimeAlt2Alt3 = standardConfig measure {
      engineASIMutable.setFunctions(OperatorType.ALT2, OperatorType.ALT3)
      engineASIMutable.propagate
   }

   println("...............starting work with ASIIM - alt3 - def ..............")
   val aimutTimeAlt3Def = standardConfig measure {
      engineASIMutable.setFunctions(OperatorType.ALT3, OperatorType.DEFAULT)
      engineASIMutable.propagate
   }

   println("...............starting work with ASIIM - alt3 - alt1 ..............")
   val aimutTimeAlt3Alt1 = standardConfig measure {
      engineASIMutable.setFunctions(OperatorType.ALT3, OperatorType.ALT1)
      engineASIMutable.propagate
   }

   println("...............starting work with ASIIM - alt3 - alt2 ..............")
   val aimutTimeAlt3Alt2 = standardConfig measure {
      engineASIMutable.setFunctions(OperatorType.ALT3, OperatorType.ALT2)
      engineASIMutable.propagate
   }

   println("...............starting work with ASIIM - alt3 - alt3 ..............")
   val aimutTimeAlt3Alt3 = standardConfig measure {
      engineASIMutable.setFunctions(OperatorType.ALT3, OperatorType.ALT3)
      engineASIMutable.propagate
   }

   println("...............starting work with ASIIM - alt4 - def ..............")
   val aimutTimeAlt4Def = standardConfig measure {
      engineASIMutable.setFunctions(OperatorType.ALT4, OperatorType.DEFAULT)
      engineASIMutable.propagate
   }

   println("...............starting work with ASIIM - alt4 - alt1 ..............")
   val aimutTimeAlt4Alt1 = standardConfig measure {
      engineASIMutable.setFunctions(OperatorType.ALT4, OperatorType.ALT1)
      engineASIMutable.propagate
   }

   println("...............starting work with ASIIM - alt4 - alt2 ..............")
   val aimutTimeAlt4Alt2 = standardConfig measure {
      engineASIMutable.setFunctions(OperatorType.ALT4, OperatorType.ALT2)
      engineASIMutable.propagate
   }

   println("...............starting work with ASIIM - alt4 - alt3 ..............")
   val aimutTimeAlt4Alt3 = standardConfig measure {
      engineASIMutable.setFunctions(OperatorType.ALT4, OperatorType.ALT3)
      engineASIMutable.propagate
   }

   // finally show ratios respect to engine tables
   println("tree ratio: " + (treeTime.value/cptTime.value))
   println("aimut ratio (def - def): " + (aimutTimeDefDef.value/cptTime.value))
   println("aimut ratio (alt1 - def): " + (aimutTimeAlt1Def.value/cptTime.value))
   println("aimut ratio (alt1 - alt1): " + (aimutTimeAlt1Alt1.value/cptTime.value))
   println("aimut ratio (alt1 - alt2): " + (aimutTimeAlt1Alt2.value/cptTime.value))
   println("aimut ratio (alt1 - alt3): " + (aimutTimeAlt1Alt3.value/cptTime.value))
   println("aimut ratio (alt2 - def): " + (aimutTimeAlt2Def.value/cptTime.value))
   println("aimut ratio (alt2 - alt1): " + (aimutTimeAlt2Alt1.value/cptTime.value))
   println("aimut ratio (alt2 - alt2): " + (aimutTimeAlt2Alt2.value/cptTime.value))
   println("aimut ratio (alt2 - alt3): " + (aimutTimeAlt2Alt3.value/cptTime.value))
   println("aimut ratio (alt3 - def): " + (aimutTimeAlt3Def.value/cptTime.value))
   println("aimut ratio (alt3 - alt1): " + (aimutTimeAlt3Alt1.value/cptTime.value))
   println("aimut ratio (alt3 - alt2): " + (aimutTimeAlt2Alt2.value/cptTime.value))
   println("aimut ratio (alt3 - alt3): " + (aimutTimeAlt2Alt3.value/cptTime.value))
   println("aimut ratio (alt4 - def): " + (aimutTimeAlt4Def.value/cptTime.value))
   println("aimut ratio (alt4 - alt1): " + (aimutTimeAlt4Alt1.value/cptTime.value))
   println("aimut ratio (alt4 - alt2): " + (aimutTimeAlt4Alt2.value/cptTime.value))
   println("aimut ratio (alt4 - alt3): " + (aimutTimeAlt4Alt3.value/cptTime.value))
}
