package access

import bnet.Bnet
import org.scalatest.FunSuite
import potential.{Potential, ValueStoreTypes}

/**
 * Class for testing the access of indices with several
 * representations
 */
class IndexAccessTest extends FunSuite {
   // defines the representations to test (those used in IJIS paper)
   val representations = List(ValueStoreTypes.TABLE,
      ValueStoreTypes.VDGLSTORE,
      ValueStoreTypes.VDILISTORE,
      ValueStoreTypes.IDPMSTORE,
      ValueStoreTypes.IDMMSTORE)

   // sets a net for testing access methods
   val bnet: Bnet = Bnet("alarm.net")

   // select one of the potentials
   val pot: Potential = bnet.potentials.maxBy(_.store.getSize._1)

   // makes conversion of pot to representations
   val converted: List[Potential] = representations.drop(0).map(representation => pot.convert(representation))

   /**
    * Tests the computation of the weights
    */
   test("Index access to potentials") {
      // considers all the indices in base potential
      val potIndices = pot.store.getIndices
      potIndices.foreach(index => {
         val baseValue = pot.store.getValue(index)
         val values = converted.map(repPot => repPot.store.getValue(index))
         println(values)
         assert(values.distinct.length == 1 && values.head == baseValue)
      })
   }
}

