package experiments.access

import bnet.Bnet

import scala.util.Random

object AccessSetUtils {
  /**
   * prepares the access set for a given net
   * @param bnet
   * @param counter
   * @return
   */
  def prepareAccessSet(bnet: Bnet, counter : Long ) : List[(Int, Long)] = {
    // for each configuration select a potential at random
    // and the corresponding index to access
    Random.setSeed(0L)
    val accessSet: List[(Int, Long)] = (0L until counter).map(index => {
      // gets the index of the potential to access
      val indexPotential = Random.nextInt(bnet.potentials.size)

      // and generates a random index as well
      val potentialSize = bnet.potentials(indexPotential.toInt).variables.possibleValues
      val indexInPotential = Math.abs(Random.nextLong()) % potentialSize

      // returns the tuple
      (indexPotential, indexInPotential)
    }).toList

    // return the access set
    accessSet
  }
}
