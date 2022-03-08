package access

import bnet.Bnet
import org.scalatest.FunSuite
import potential.indexBased.IDPIStore
import potential.{Potential, ValueStoreTypes}

/**
 * Class for testing the access of indices with several
 * representations
 */
class IDPIStoreAccessTest extends FunSuite {
   // defines the representations to test (those used in IJIS paper)
   val representations = List(ValueStoreTypes.TABLE,
      ValueStoreTypes.IDPISTORE)

   // sets a net for testing access methods
   val bnet: Bnet = Bnet("mildew.net")

   /**
    * Tests the access to a set
    */
   test("Index access to potentials with method 1") {
      // considers all the potentials
      for(potBase <- bnet.potentials){
         val potConverted = potBase.convert(representations(1))
         val indices = potBase.store.getIndices
         indices.foreach(index => {
            val value1 = potBase.store.getValue(index)
            val value2 = potConverted.store.getValue(index)

            // assert equality with both values
            assert(value1 == value2)
         })
      }
   }
}

