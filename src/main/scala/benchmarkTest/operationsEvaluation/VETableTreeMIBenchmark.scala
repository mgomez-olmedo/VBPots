package benchmarkTest.operationsEvaluation

import bnet.Bnet
import inference.VariableElimination
import org.scalameter._
import potential.{OperatorType, ValueStoreTypes}

/**
  * Object to perform comparisons in the application of
  * VariableElimination algorithm on a given network and
  * using different alternatives for AIIM alternatives
  * for combination and marginalization
  */
object VETableTreeMIBenchmark extends App {

   // defines the configuration for benchmarking
   val standardConfig = config(
      Key.exec.minWarmupRuns -> 20,
      Key.exec.maxWarmupRuns -> 40,
      Key.exec.benchRuns -> 25,
      Key.verbose -> true
   ) withWarmer new Warmer.Default

   // define the net name to propagate
   var netName = "alarm"
   var extension="net"

   // creates the engine for tables
   val bnet = Bnet(netName + "." + extension)
   val engineTables=new VariableElimination(bnet,false)

   // creates a engine for tree
   val bnetTree = Bnet.convert(bnet, ValueStoreTypes.TREE)
   val engineTree = new VariableElimination(bnetTree, false)

   // creates a engine for Map indices
   val bnetMapIndices = Bnet.convert(bnet, ValueStoreTypes.IDMMSTORE)
   val engineAIMutable = new VariableElimination(bnetMapIndices, false)

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

   println("...............starting work with AIIM - def - def ..............")
   val aimutTimeDefDef = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt1 - def ..............")
   val aimutTimeAlt1Def = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT1, OperatorType.DEFAULT)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt1 - alt1 ..............")
   val aimutTimeAlt1Alt1 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT1, OperatorType.ALT1)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt1 - alt2 ..............")
   val aimutTimeAlt1Alt2 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT1, OperatorType.ALT2)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt1 - alt3 ..............")
   val aimutTimeAlt1Alt3 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT1, OperatorType.ALT3)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt2 - def ..............")
   val aimutTimeAlt2Def = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT2, OperatorType.DEFAULT)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt2 - alt1 ..............")
   val aimutTimeAlt2Alt1 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT2, OperatorType.ALT1)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt2 - alt2 ..............")
   val aimutTimeAlt2Alt2 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT2, OperatorType.ALT2)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt2 - alt3 ..............")
   val aimutTimeAlt2Alt3 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT2, OperatorType.ALT3)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt3 - def ..............")
   val aimutTimeAlt3Def = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT3, OperatorType.DEFAULT)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt3 - alt1 ..............")
   val aimutTimeAlt3Alt1 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT3, OperatorType.ALT1)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt3 - alt2 ..............")
   val aimutTimeAlt3Alt2 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT3, OperatorType.ALT2)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt3 - alt3 ..............")
   val aimutTimeAlt3Alt3 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT2, OperatorType.ALT3)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt4 - def ..............")
   val aimutTimeAlt4Def = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT4, OperatorType.DEFAULT)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt4 - alt1 ..............")
   val aimutTimeAlt4Alt1 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT4, OperatorType.ALT1)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt4 - alt2 ..............")
   val aimutTimeAlt4Alt2 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT4, OperatorType.ALT2)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt4 - alt3 ..............")
   val aimutTimeAlt4Alt3 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT4, OperatorType.ALT3)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt5 - def ..............")
   val aimutTimeAlt5Def = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT5, OperatorType.DEFAULT)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt5 - alt1 ..............")
   val aimutTimeAlt5Alt1 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT5, OperatorType.ALT1)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt5 - alt2 ..............")
   val aimutTimeAlt5Alt2 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT5, OperatorType.ALT2)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt5 - alt3 ..............")
   val aimutTimeAlt5Alt3 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT5, OperatorType.ALT3)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt6 - def ..............")
   val aimutTimeAlt6Def = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT6, OperatorType.DEFAULT)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt6 - alt1 ..............")
   val aimutTimeAlt6Alt1 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT6, OperatorType.ALT1)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt6 - alt2 ..............")
   val aimutTimeAlt6Alt2 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT6, OperatorType.ALT2)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt6 - alt3 ..............")
   val aimutTimeAlt6Alt3 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT6, OperatorType.ALT3)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt7 - def ..............")
   val aimutTimeAlt7Def = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT7, OperatorType.DEFAULT)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt7 - alt1 ..............")
   val aimutTimeAlt7Alt1 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT7, OperatorType.ALT1)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt7 - alt2 ..............")
   val aimutTimeAlt7Alt2 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT7, OperatorType.ALT2)
      engineAIMutable.propagate
   }

   println("...............starting work with AIIM - alt7 - alt3 ..............")
   val aimutTimeAlt7Alt3 = standardConfig measure {
      engineAIMutable.setFunctions(OperatorType.ALT7, OperatorType.ALT3)
      engineAIMutable.propagate
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
   println("aimut ratio (alt3 - alt2): " + (aimutTimeAlt3Alt2.value/cptTime.value))
   println("aimut ratio (alt3 - alt3): " + (aimutTimeAlt3Alt3.value/cptTime.value))
   println("aimut ratio (alt4 - def): " + (aimutTimeAlt4Def.value/cptTime.value))
   println("aimut ratio (alt4 - alt1): " + (aimutTimeAlt4Alt1.value/cptTime.value))
   println("aimut ratio (alt4 - alt2): " + (aimutTimeAlt4Alt2.value/cptTime.value))
   println("aimut ratio (alt4 - alt3): " + (aimutTimeAlt4Alt3.value/cptTime.value))
   println("aimut ratio (alt5 - def): " + (aimutTimeAlt5Def.value/cptTime.value))
   println("aimut ratio (alt5 - alt1): " + (aimutTimeAlt5Alt1.value/cptTime.value))
   println("aimut ratio (alt5 - alt2): " + (aimutTimeAlt5Alt2.value/cptTime.value))
   println("aimut ratio (alt5 - alt3): " + (aimutTimeAlt5Alt3.value/cptTime.value))
   println("aimut ratio (alt6 - def): " + (aimutTimeAlt6Def.value/cptTime.value))
   println("aimut ratio (alt6 - alt1): " + (aimutTimeAlt6Alt1.value/cptTime.value))
   println("aimut ratio (alt6 - alt2): " + (aimutTimeAlt6Alt2.value/cptTime.value))
   println("aimut ratio (alt6 - alt3): " + (aimutTimeAlt6Alt3.value/cptTime.value))
   println("aimut ratio (alt7 - def): " + (aimutTimeAlt7Def.value/cptTime.value))
   println("aimut ratio (alt7 - alt1): " + (aimutTimeAlt7Alt1.value/cptTime.value))
   println("aimut ratio (alt7 - alt2): " + (aimutTimeAlt7Alt2.value/cptTime.value))
   println("aimut ratio (alt7 - alt3): " + (aimutTimeAlt7Alt3.value/cptTime.value))
}
