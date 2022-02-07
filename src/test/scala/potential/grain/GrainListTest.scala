package potential.grain

import org.scalatest.FunSuite

class GrainListTest extends FunSuite{
   val list1 = List[Grain](new Grain(7, 8))
   val grainList1 = new GrainList(list1)
   val list2 = (new Grain(9,9)) :: List[Grain](new Grain(6,6))
   val grainList2 = new GrainList(list2)

   test("checks a merge producing a single grain"){
      val result = GrainList.merge(grainList1, grainList2)
      println(result)
      assert(result.size == 1)
   }
}
