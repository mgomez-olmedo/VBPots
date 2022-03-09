package experiments.access

import bnet.Bnet
import org.scalameter.{Key, Warmer, config}
import potential.ValueStoreTypes
import potential.indexBased.IDPIStore

object IDPIStoreAccessBenchmark extends App{

  // defines experiment configuration
  val standardConfig = config(
    Key.exec.maxWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  // sets the number of indices to access
  val counter = 10000

  // sets the net to use for testing
  val netName = "mildew.net"

  // read the net and convert into the target store
  val bnet = Bnet(netName)
  val convertedBnet = Bnet.convert(bnet, ValueStoreTypes.IDPISTORE)

  // prepare the access set
  val accessSet = AccessSetUtils.prepareAccessSet(convertedBnet, counter)
  println("access set length:  " + accessSet.length)

  // measure time with first alternative
  var result1 = List[Double]()
  val time1 = standardConfig measure {
    // perform the measure as many times as iterations shows
    for (i <- 0L until accessSet.length) {
      // gets the potential and index to access
      val target: (Int, Long) = accessSet(i.toInt)
      val potential = convertedBnet.potentials(target._1.toInt)
      result1 = potential.store.getValue(target._2) :: result1
    }
  }

  // measure time with second alternative
  var result2 = List[Double]()
  val time2 = standardConfig measure {
    // perform the measure as many times as iterations shows
    for (i <- 0L until accessSet.length) {
      // gets the potential and index to access
      val target: (Int, Long) = accessSet(i.toInt)
      val potential = convertedBnet.potentials(target._1.toInt)
      val store = potential.store.asInstanceOf[IDPIStore]
        result2 = store.getValue2(target._2) :: result2
    }
  }

  // show times
  println("time with method 1: " + time1)
  println("time with method 2: " + time2)

  // check the differences between result1 and result2
  val comparisons = (0 until counter).map(index => result1(index) == result2(index))
  println("comparison flags: " + comparisons.distinct)

  // shows result vector lengths
  //println("result1 length: " + result1.length)
  //println("result2 length: " + result2.length)

  println("resultado1: " + result1)
  println("resultado2: " + result2)
}
