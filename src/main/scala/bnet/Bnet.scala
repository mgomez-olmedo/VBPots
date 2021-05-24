package bnet

import java.io._
import base.{Variable, VariableSet}
import graph.Graph
import parser.{BnetParser, UAIDirectParsing, UAINetParser}
import potential.{Potential, ValueStoreTypes}
import utils.{DataSizes, Serializator}

import scala.util.Random

/**
 * Class for representing Bayesian Networks
 * @constructor creates an instance of the class receiving as
 * arguments:
 * @param name name of the Bayesian network
 * @param variables set of variables of the network
 * @param potentials quantitative information about the net
 */
class Bnet(val name: String, val variables: VariableSet,
           val potentials: List[Potential]) extends Serializable {

   // graph for performing d-separation analyses
   val graph: Graph = Graph(this)

   /**
    * Gets the potential for a given variable
    * @param variable target variable
    * @return potential with quantitative information for the
    *         variable passed as argument
    */
   def getPotentialForVariable(variable: Variable): Potential =
      potentials.find(potential =>
         potential.isPotentialForParentVariable(variable)).orNull

   /**
    * Gets the potential for a variable giving its name
    * @param variableName name of target variable
    * @return potential for the variable which name is passed
    *         as argument
    */
   def getPotentialForVariable(variableName: String): Potential = {
      getPotentialForVariable(variables.getVariable(variableName))
   }

   /**
    * Gets the size of the object
    * @return estimated memory size of the object
    */
   def getMemorySize: Long = {
      // get variable sizes
      val variableSizes = variables.getMemorySize

      // get name size
      val nameSize = DataSizes.stringSize(name)

      // get potentials size
      val potentialSizes = potentials.
         map(potential => potential.getMemorySize).sum

      // return the sum of the sizes
      variableSizes + potentialSizes + nameSize
   }

   /**
    * just compute the memory size of potentials, ignoring
    * variables and net name
    * @return potentials memory size
    */
   def getPotentialsMemorySize: Long = {
      potentials.
         map(potential => potential.getMemorySize).sum
   }

   /**
    * Gets general info about the network
    * @return tuple with
    *         - number of variables
    *         - number of arcs
    *         - number of states for all variables
    *         - avg number of states
    *         - min number of states
    *         - max number of states
    */
   def getGeneralInfo: (Int, Int, Int, Double, Int, Int) = {
      // get the number of variables
      val numberVariables = variables.getSize

      // gets the number of arcs
      val arcs = potentials.map(potential =>
         potential.variables.getSize - 1).sum

      // gets the number of states for all the variables
      val numberStates = variables.map(variable =>
         variable.getNumberStates).sum

      // gets average number of states
      val averageStates = numberStates / (numberVariables * 1.0)

      // gets minimum and maximum number of states
      val minStates = variables.map(variable => variable.getNumberStates).min
      val maxStates = variables.map(variable => variable.getNumberStates).max

      // just compose a tuple with all this info
      (numberVariables, arcs, numberStates, averageStates, minStates,
         maxStates)
   }
}

/**
 * Companion object
 */
object Bnet {
   /**
    * Definition of Info produced for potentials analisys
    */
   type InfoPotential = List[(VariableSet, List[Double], Double, List[Double],
      List[Double], Double, Long, Long)]

   /**
    * Apply method for making new objects
    * @param netName name of the bnet
    * @param nodes list of nodes (each node is represented by a name
    *              and a list of states)
    * @param potentialsList list of potentials. Each potential is
    *                       represented by a variable (owning the
    *                       potential, a list of parent variables
    *                       and a list of values)
    * @return Bnet object created
    */
   def apply(netName: String, nodes: List[(String, List[String])],
             potentialsList: List[((String, List[String]),
                List[Double])]): Bnet = {

      /**
       * Data member to store the list of potentials
       */
      var potentialObjects: List[Potential] = List[Potential]()

      /**
       * Data member to store the list of variables
       */
      var variableList: Map[String, Variable] = Map[String, Variable]()

      // Considers each potential
      potentialsList.foreach(_ => {
         // Creates a map with all the variables
         variableList = {
            nodes.map(x => x._1 -> new Variable(x._1, x._2)).toMap
         }

         // Make potential objects
         potentialObjects = potentialsList.map(x => {
            val potentialVariable: String = x._1._1
            val potentialParents: List[String] = x._1._2
            val potentialVariables: List[String] =
               potentialParents ::: List(potentialVariable)
            val nullVariable = new Variable("None", List())
            val potentialVariableSet: List[Variable] = {
               potentialVariables.
                  map(x => variableList.getOrElse(x, nullVariable))
            }

            // Now makes the potentials
            Potential(new VariableSet(potentialVariableSet),
               x._2, ValueStoreTypes.TABLE)
         })
      })

      // creates a Bnet object and return it
      new Bnet(netName, new VariableSet(variableList.values.toList),
         potentialObjects)
   }

   /**
    * Direct method for creating a new net given its corresponding
    * data members
    * @param netName name of net to generate
    * @param variableSet set of variables of bnet
    * @param potentialsList list of potentials with quantitative info
    * @return Bnet object created
    */
   def apply(netName: String, variableSet: VariableSet,
             potentialsList: List[Potential]): Bnet = {
      new Bnet(netName, variableSet, potentialsList)
   }

   /**
    * Method for making objects reading from files
    * @param netName name of net to create
    * @return Bnet created object
    */
   def apply(netName: String): Bnet = {
      // gets the extension from netName
      val extension = netName.split("\\.").last

      // determine the folder
      val folder = getFolder(extension)

      // gets the net parsing the corresponding file
      val net = extension match {
         case "net" =>
            val parserNet = new BnetParser
            val reader = new FileReader(folder + netName)
            parserNet.parseAll(parserNet.component, reader).get
         case "uai" =>
            val parserUAI = new UAINetParser
            val reader = new FileReader(folder + netName)
            parserUAI.parseAll(parserUAI.component, reader).get
         case "uaix" =>
            val folder = "./data/UAI/selected/"
            UAIDirectParsing.process(folder + netName)
      }

      // return net
      net
   }

   /**
    * Method for analyzing the information about potentials
    * @return tuple with
    *         - domain of the potential (VariableSet)
    *         - list of values
    *         - number of values stored
    *         - list of different values
    *         - proportions of repetitions
    *         - zero proportions
    *         - sizes measures:
    *            * CPT: (size, size)
    *            * PT, PPT: (leaves, internals)
    *            * stores: (values - grains) or (values - indices)
    */
   def analyzePotentials(bnet: Bnet): Bnet.InfoPotential = {
      // Analyze each potential
      bnet.potentials.map(x => x.analyze)
   }

   /**
    * Method for analyzing the potentials of a list
    * @param potentials list of potentials to analyze
    * @return list with the result of the analysis for each
    *         potential in the list passed as argument
    */
   def analyzePotentials(potentials: List[Potential]): Bnet.InfoPotential = {
      potentials.map(x => x.analyze)
   }

   /**
    * Serialization method for writing an object to a file
    * @param fileName name of file to create
    * @param bnet bnet to store
    */
   def writeObject(fileName: String, bnet: Bnet): Unit = {
      //val file=new FileOutputStream(fileName)
      //val stream=new ObjectOutputStream(file)
      //stream.writeObject(bnet)
      Serializator.writeObject[Bnet](fileName, bnet)
   }

   /**
    * Serialization method for reading objects from files
    * @param fileName name of file tor ead
    * @return object with the information taken from the file
    */
   def readObject(fileName: String): Bnet = {
      // gets the extension
      val extension = fileName.split("\\.").last

      // gets the folder
      val folder = getFolder(extension)

      // read the object
      Serializator.readObject[Bnet](folder + fileName)
   }

   /**
    * General method of conversion to get the same net with
    * another representation for potentials
    * @param net net to convert
    * @param storeType type of storage to use
    * @return created object
    */
   def convert(net: Bnet, storeType: ValueStoreTypes.Value): Bnet = {
      val pots = net.potentials.map(potential => {
         val pot = potential.convert(storeType)
         pot
      })

      // return a new net with the potentials converted to the
      // desired type
      new Bnet(net.name, net.variables, pots)
   }

   /**
    * determines the folder analyzing the extension of
    * the file
    */
   private def getFolder(extension: String): String = {
      val folder = extension match {
         case "net" =>
            "./data/bnlearn/"
         case "uai" =>
            "./data/UAI/selected/"
         case "uaix" =>
            "./data/UAI/selected/"
      }

      // return folder
      folder
   }

   /**
    * prepares the indexes of the variables to propagate
    * @param bnet target network
    * @param counter number of varfiables to select
    * @param seed seed for random number generation
    * @return
    */
   def randomSelectVariableIndexes(bnet: Bnet, counter : Int, seed : Long) : List[Int] = {
      // for each pair select two potentials
      Random.setSeed(seed)

      // shuffle potential variables
      val indexes = Random.shuffle((0 until bnet.variables.size).toList)

      // takes first variables (as indicated by counter)
      val selectedIndexes: List[Int] = indexes.take(counter)

      // return the list of selected indexes
      selectedIndexes
   }
}
