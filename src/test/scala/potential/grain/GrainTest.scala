package potential.grain

import org.scalatest.FunSuite

class GrainTest extends FunSuite{
   val grain1 = new Grain(3, 7)
   val grain2 = new Grain(8, 10)
   val grain3 = new Grain(1, 2)

   // just checks a merge with end - start match
   test("checks merge with end - start match") {
      if(grain1.isConsecutiveGrain(grain2)){
         val newGrain = Grain.merge(grain1, grain2)
         assert(newGrain.start == 3 && newGrain.end == 10)
      }
      else{
         assert(false)
      }
   }

   // just checks a merge with a start - end match
   test("checks merge with start - end match") {
      if(grain1.isConsecutiveGrain(grain3)){
         val newGrain = Grain.merge(grain1, grain3)
         assert(newGrain.start == 1 && newGrain.end == 7)
      }
      else{
         assert(false)
      }
   }
}
