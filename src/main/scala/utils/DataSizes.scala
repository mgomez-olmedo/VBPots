package utils

/**
  * Object for storing data sizes used for memory size
 * estimation
  */
object DataSizes {
   final val BOOLEAN = 1
   final val BYTE = 1
   final val SHORT = 2
   final val CHAR = 2
   final val INT = 4
   final val LONG = 4
   final val FLOAT = 4
   final val DOUBLE = 8
   final val REFERENCE = 8
   final val VARIABLE = 50
   final val ARRAY = REFERENCE*2
   final val SET=4*REFERENCE
   final val MAP=8*REFERENCE
   final def stringSize(string : String): Int = {
      string.length * CHAR
   }
}

