package base

import utils.{DataSizes, Serializator}

import scala.util.Random

/**
  * Class for representing DISCRETE variables
  * @constructor creates a Variable object from the following
  *              data
  * @param name name of the variable
  * @param states list of states for the variable
  */
class Variable(val name: String, val states: List[String]) extends Serializable{
   /**
     * Method returning the number of states
     * @return number of states of the variable represented
     *         by the object
     */
   def getNumberStates : Int = states.size

   /**
     * Gets a string with relevant information
     * @return string with relevant information about the
     *         variable: name and number of states
     */
   override def toString: String = {
      "name: " + name  + " states: " + getNumberStates
   }

   /**
     * Checks if two variables are equals (both have the same
     * name)
     * @param other variable object to compare with
     * @return boolean result
     */
   override def equals(other: Any): Boolean =
      other match {
         case other: Variable => name.equals(other.name)
         case _ => false
      }

   /**
     * Gets memory size for an object of variable type
     * @return value of memory size estimation for a variable
     *         (given by a constant)
     */
   def getMemorySize: Int = {
      //val stringSize=DataSizes.stringSize(name)
      //val numberStatesSize = DataSizes.INT
      //val statesSize = states.map(state => DataSizes.stringSize(state)).sum + DataSizes.ARRAY
      //(stringSize + numberStatesSize + statesSize).toLong
      DataSizes.VARIABLE
   }

   /**
    * overrides hashCode for considering the name and
    * the number of states
    * @return
    */
   override def hashCode(): Int = {
      (name + getNumberStates).hashCode()
   }
}

/**
  * Companion object for providing serialization service
  */
object Variable{
   /**
     * apply method
     * @param name variable name
     * @param states number of states
     * @return variable object created
     */
   def apply(name:String, states:List[String]): Variable = {
      new Variable(name, states)
   }

   /**
     * Creates a new variable with an id and a number of states
     * @param id id for the variable to create
     * @param numberStates number of states of the variable to create
     * @return variable object created
     */
   def apply(id : Int, numberStates : Int) : Variable = {
      val stateNames = (0 until numberStates).
                           map(index => "s"+index).toList
      new Variable("X"+id, stateNames)
   }

   /**
     * Serialization method for writing an object to a file
     * @param fileName name of the file with object information
     * @param variable variable object to serialize
     */
   def writeObject(fileName : String, variable: Variable): Unit = {
      //val file=new FileOutputStream(fileName)
      //val stream=new ObjectOutputStream(file)
      //stream.writeObject(variable)
      Serializator.writeObject[Variable](fileName, variable)
   }

   /**
     * Serialization method for reading objects from files
     * @param fileName name of the file to read from
     * @return Variable object created from data file
     */
   def readObject(fileName : String) : Variable = {
      //val file=new FileInputStream(fileName)
      //val stream=new ObjectInputStream(file)
      //stream.readObject().asInstanceOf[Variable]
      Serializator.readObject[Variable](fileName)
   }

   /**
     * Generates a new variable with X as general name and
     * the index as sub-index. State names will be x sub-index
     * (from 1 to the desired number of states)
     * @param index sub-index to add to stated prefix: X
     * @param numStates number of states of the variable to generate
     * @return generated variable
     */
   def generate(index : Int, numStates : Int) : Variable = {
      // generates the list of names for states
      val statesNames = (1 to numStates).map(ind => "x_"+index+"_"+ind).toList

      // just return the variable with "X_index" as name
      Variable("X"+index, statesNames)
   }

   /**
     * Generates a new variable with X_index as name and a
     * random number of states, limited by maxCardinality
     * @param index index to add
     * @param maxCardinality value of maximal cardinality for
     *                       the new variable
     * @return object created
     */
   def generateWithRandomCardinality(index : Int,
                                     maxCardinality : Int) : Variable = {
      // generate a variable with a random number of states,
      // from 2 to maxCardinality
      generate(index, Random.nextInt(maxCardinality-1)+2)
   }
}
