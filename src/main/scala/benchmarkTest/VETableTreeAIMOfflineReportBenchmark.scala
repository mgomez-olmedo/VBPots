package benchmarkTest

import bnet.Bnet
import inference.VariableElimination
import org.scalameter
import org.scalameter._
import org.scalameter.api.Gen
import org.scalameter.execution.SeparateJvmsExecutor
import org.scalameter.picklers.Implicits._
import potential.{OperatorType, ValueStoreTypes}

/**
  * Object to perform comparisons in the application of
  * VariableElimination algorithm on a given network and
  * using different representations for potentials
  */
object VETableTreeAIMOfflineReportBenchmark extends Bench.OfflineReport {

   /**
     * makes tests in a separated thread
     */
   override lazy val executor: SeparateJvmsExecutor[Double] = SeparateJvmsExecutor(
      new Executor.Warmer.Default,
      Aggregator.max,
      //new Executor.Measurer.MemoryFootprint
      new scalameter.Executor.Measurer.Default
   )
   override lazy val persistor = new persistence.SerializationPersistor

   // define the net name to propagate
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
   val engineAIMutable = new VariableElimination(bnetMapIndices, false)

   // defines the number of repetitions for each propagation
   val ranges: Gen[Range] = for {
      size <- Gen.range("iter")(1, 10, 1)
   } yield 0 until size

   // general test: propagation with CPT, PTs and AIMut
   // starts with CPT representation
   engineTables.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
   performance of "Propagation time" in {
      measure method "CPT" in {
         using(ranges) in {
            _ => engineTables.propagate
         }
      }

      // propagation with PC representation
      engineTree.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
      measure method "PT" in {
         using(ranges) in {
            _ => engineTree.propagate
         }
      }

      // propagation with AIMut representation
      engineAIMutable.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
      measure method "AIMut" in {
         using(ranges) in {
            _ => engineAIMutable.propagate
         }
      }
   }
}
