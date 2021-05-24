package mapper

import base.{Variable, VariableSet}
import org.scalatest.FunSuite

class CombineMapperTest2 extends FunSuite {
   val variable1 = new Variable("X1", List("x11", "x12"))
   val variable2 = new Variable("X2", List("x21", "x22", "x23"))
   val variable3 = new Variable("X3", List("x31", "x32", "x33", "x34"))
   val variable4 = new Variable("X4", List("x41", "x42"))
   val variable5 = new Variable("X5", List("x51", "x52", "x53"))
   val variable6 = new Variable("X6", List("x61", "x62", "x63", "x64"))

   // Creates a variable set with variable1, variable2, variable3 and variable4
   val variableSet1 = new VariableSet(List(variable1, variable2, variable3,
                                          variable4, variable5))
   println("First object: ")
   println(variableSet1)

   // Creates a variable set with variable3, variable5 and variable2
   val variableSet2 = new VariableSet(List(variable3, variable5, variable2,
                                           variable6))
   println("Second object: ")
   println(variableSet2)

   // now creates a mapper for combine
   // the result potential will be formed by X1, X2, X3, X4, X5, X6
   // the cardinalities of the variables are 2, 3, 4, 2, 3, 4
   // the weights in result variables are 288, 96, 24, 12, 4, 1
   // the configuration X1=x12, X2=x22, X3=x32, X4=x42, X5=x52, X6=x62
   // will have the index 1*288 + 1*96 + 1*24 + 1*12 + 1*4 + 1 = 425
   val mapper = new CombineMapper(variableSet1, variableSet2)

   val result = mapper.mapIndices(425)
   print(result)
}
