package potential.grain

import org.scalatest.FunSuite

class GrainListTest extends FunSuite{
   val list1 = List[Grain](new Grain(7, 8))
   val grainList1 = new GrainList(list1)
   val list2 = (new Grain(9,9)) :: List[Grain](new Grain(6,6))
   val grainList2 = new GrainList(list2)

   test("checks a merge producing a single grain"){
      val result = GrainList.merge(grainList1, grainList2)
      assert(result.size == 1)
   }

   test("check of add grains to a previous list"){
      val baseList = new GrainList(List[Grain](new Grain(7,8), new Grain(11,11), new Grain(0,0)))
      val grain1 = new Grain(5,6)

      // perform first operation
      val result1 = baseList.addGrain(grain1)

      // perform second operation
      val grain2 = new Grain(9, 10)
      val result2 = result1.addGrain(grain2)

      // perform third operation
      val grain3 = new Grain(1, 3)
      val result3 = result2.addGrain(grain3)

      // checks thew result contains only two grains
      assert(result3.size ==2)
   }
}
