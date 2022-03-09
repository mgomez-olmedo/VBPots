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
      VDGSSTORE,    // Value based - map value - set of grains with indices
      VDGLSTORE,    // Value based - map value - list of grain with indices
      VDILMSTORE,   // Value based - map value - mutable list of indices
      VDILISTORE, // Value based - map value - immutable list of indices
      VDISMSTORE,   // Value based - map value - mutable set of indices
      VDISISTORE,   // Value based - map value - immutable set of indices
      IDPMSTORE,    // Index based - mutable array of values, array of pairs (indexp, indexv)
                                    // with indexv - index of related value in the
                                    // list of values. indexp: index in potential
      IDPISTORE,   // Index based - immutable array of values, list of pairs (indexp, indexv)
                           // with indexv - index of related value in the
                           // array of values. indexp: index in potential
      IDSMSTORE,   // Index based - mutable array of sets of indices and array of values
      IDSISTORE,   // Index based - immutable sets of indices and array of values
      IDMMSTORE    // Index based - mutable map index - index in values array (mutable)s
      = Value
}