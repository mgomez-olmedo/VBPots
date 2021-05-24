package experiments

import inference.{QualitativeVariableElimination, VariableElimination}
import potential.{Potential, ValueStoreTypes}

/**
 * Object extending of App for executing test about
 * QualitativeVariableElimination algorithm execution
 */
object QualitativeVariableEliminationTest extends App {
   /**
    * names of networks to propagate
    */
   val netName = "diabetes"

   /**
    * General extension for nets to analyze
    */
   val extension = "net"

   println("net: " + netName)
   println()

   // creates the engine for tables
   println("  ...............................................")
   val engine = new QualitativeVariableElimination(netName, extension)
   engine.propagate

   // shows the costs for each variable
   println(engine.showTopMaxCosts(20))
}
