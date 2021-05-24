package potential

import potential.DefaultValueComputerType.DefaultValueComputerType
import utils.Util

/**
 * Enumeration including the available options for default
 * value computer
 */
object DefaultValueComputerType extends Enumeration {
   type DefaultValueComputerType = Value
   val REGULAR, ZERO = Value
}

/**
 * Base trait for default value computation
 */
trait DefaultValueComputer extends Serializable {
   /**
    * value acting as default value
    */
   var defaultValue: Double

   /**
    * computes the default value analyzing the values
    * passed as argument
    *
    * @param values sequence of values to analyze
    * @return default value
    */
   def computeDefaultValue(values: Seq[Double]): Double

   /**
    * gets the default value
    * @return default value
    */
   def getDefaultValue: Double = defaultValue

   /**
    * gets the kind of default value behavior
    */
   def getType: DefaultValueComputerType
}

/**
 * Trait with determination of default value through
 * the analysis of the sequence of values
 */
class RegularDefaultValueComputer extends DefaultValueComputer {
   /**
    * default value to use
    */
   override var defaultValue: Double = _

   /**
    * modified version of default value computation for
    * analyzing the sequence of values
    * @param values sequence of values to analyze
    * @return default value
    */
   override def computeDefaultValue(values: Seq[Double]): Double = {
      defaultValue = Util.determineDefaultValue(values)
      defaultValue
   }

   /**
    * Shows the kind of computation for default values
    *
    * @return kind of default value behavior
    */
   override def getType: DefaultValueComputerType =
      DefaultValueComputerType.REGULAR
}

/**
 * Default value treatment using 0 as default value without
 * further analysis of the sequence of values
 */
class ZeroDefaultValueComputer extends DefaultValueComputer {
   /**
    * default value container
    */
   override var defaultValue: Double = _

   /**
    * sets default value to 0
    * @param values sequence of values (it is not analyzed)
    * @return defaultValue (0.0)
    */
   override def computeDefaultValue(values: Seq[Double]): Double = {
      defaultValue = 0.0
      defaultValue
   }

   /**
    * Shows the kind of computation for default values
    *
    * @return default value type
    */
   override def getType: DefaultValueComputerType =
      DefaultValueComputerType.ZERO
}

/**
 * Factory object for default value behavior
 */
object DefaultValueComputer {
   /**
    * factory method
    * @param defaultValueComputerType kind of default value behavior
    *                                 to use
    * @return created object
    */
   def apply(defaultValueComputerType: DefaultValueComputerType):
                                             DefaultValueComputer = {
      defaultValueComputerType match {
         case DefaultValueComputerType.REGULAR =>
                              new RegularDefaultValueComputer
         case DefaultValueComputerType.ZERO =>
                              new ZeroDefaultValueComputer
      }
   }
}
