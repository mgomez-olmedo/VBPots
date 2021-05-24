package potential

/**
 * Object acting as enumeration for types of operators
 * for combination and marginalization
 */
object OperatorType extends Enumeration{
   type OperatorType = Value
   val DEFAULT,
      ALT1,
      ALT2,
      ALT3,
      ALT4,
      ALT5,
      ALT6,
      ALT7,
      ALT8,
      ALT9 = Value
}

/**
  * Enumeration for the kinds of trees to manage
  */
object ValueStoreTypes extends Enumeration{
   type ValueStoreType = Value
   val TABLE,              // CPTs usual representation (TableStore)
      TREE,                // normal tree representation (TreeStore)
      PRUNEDTREE,          // trees with a prune operation after creation
      VDGSET,         // map value - set of grains with indices
      VDGLIST,        // map value - list of grain with indices
      VDILISTMUT,   // map value - list of indices (mutable)
      VDILISTIMMUT, // map value - list of indices (inmutable)
      VDISETMUT,    // map value - set of indices (mutable)
      VDISETIMMUT,  // map value - set of indices (immutable)
      IDPMUT,     // array of values, array of pairs (long, long)
                           // with index - index of related value in the
                           // list of values (mutable - ArrayBuffer)
      IDPIMMUT,   // array of values, list of pairs (long, long)
                           // with index - index of related value in the
                           // array of values (inmutable - Array)
      IDSETMUT,  // array set of indices and array of values
                           // (mutable)
      IDSETIMMUT, // array set of indices and array of values
                           // (immutable)
      IDMMUT        // map index - index in values array (mutable)s
      = Value
}