package potential

import base.Variable

/**
  * Object for creating the definitions of operations for
  * combination and marginalization
  */
object Operations {
   /**
     * Definition of the function for performing the combination
     * of potentials
     */
   type Combination = (ValueStore, ValueStore) => ValueStore

   /**
     * Definition of function for performing marginalization
     */
   type Marginalization = (ValueStore, Variable) => ValueStore
}
