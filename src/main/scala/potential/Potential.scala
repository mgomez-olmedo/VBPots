package potential

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import base.{Configuration, Variable, VariableSet}
import potential.ValueStoreTypes.ValueStoreType
import potential.indexBased.{IDMMStore, IDPIStore, IDPMStore, IDSIStore, IDSMStore}
import potential.valueBased.{VDGLStore, VDGSStore, VDILIStore, VDILMStore, VDISIStore, VDISMStore}
import utils.Util

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Class for representing potentials
  * @constructor creates an instance of Potential object
  * @param store value store object containing the
 *              values of the potential
  */
class Potential(val store: ValueStore) extends Serializable {
   /**
     * data member for storing the variables (obtained from
    * values)
     */
   val variables: VariableSet = store.getVariables

   /**
     * stores information about mainVar (last one)
     */
   val mainVariable: Variable = variables.last

   /**
     * stores information about conditioning vars. The information
     * is stored as a list of variables
     */
   val conditioningVars: List[Variable] =
         if(variables.size > 1) variables.dropRight(1).toList
         else List()

   /**
    * forces the store reassignment of the combination and
    * marginalization functions according to ValueStore
    * combinationType and marginalizationType data
    */
   def setFunctions(combType:OperatorType.OperatorType,
                   margType:OperatorType.OperatorType) : Unit = {
      store.setCombiner(combType)
      store.setMarginalizer(margType)
   }

   /**
     * method for analyzing potential info
     * @return tuple with information about the potential
     *   - domain of the potential (VariableSet)
     *   - list of values stored
     *   - number of values stored
     *   - list of different values
     *   - proportions of repetitions
     *   - zero proportions
     *   - sizes measures:
     *      * CPT: (size, size)
     *      * PT, PPT: (leaves, internals)
     *      * stores: (values - grains) or (values - indices)
     */
   def analyze: (VariableSet, List[Double], Double, List[Double],
                  List[Double], Double, Long, Long) = {
      // gets the list of values from values data member
      val concreteValues=store.getListValues

      // removes repetitions in the list of values
      val differentValues: List[Double] = store.getDifferentValues

      // gets the proportion of zeros
      val zeros:Double=store.getZerosProportion

      // computes the proportions for each value
      val proportions: List[Double] = store.getValuesProportions

      //  computes the sizes
      val sizes: (Long, Long) = store match{
         case TableStore(_, values) =>
            (values.length, values.length)

         case TreeStore(_, _) =>
            (0, 0)
      }

      // return the result of the analysis: variables, values,
      // number of values, values without repetitions, proportions
      // of repetitions, zero proportions, and finally two measures
      // of sizes with different meaning according to the type of
      // potential: tables (size, size), trees (leaves, internals),
      // stores (values, grains) or (values, indices)
      (variables, concreteValues, concreteValues.size, differentValues,
         proportions, zeros, sizes._1, sizes._2)
   }

   /**
     * Gets some measures about the patterns in the data
     * @return tuple with:
     *         - number of different values
     *         - default value (most repeated one)
     *         - counter of occurrences of default value
     *         - max percentage of repetitions
     *         - average percentage of repetitions
     *         - min percentage of repetitions
     *         - counter of changes
     */
   def getPatternMeasures: (Int, Double, Double, Double, Double, Double, Double) = {
      val values = store.getListValues
      val counters: Map[Double, Int] = if(values.nonEmpty)
         values.groupBy(value => value).
            map(entry => (entry._1, entry._2.length)) else Map(0.0->1)

      // computes the number of different values
      val differentValues: Int = values.distinct.size

      // gets the default value
      val defaultValueEntry: (Double, Int) = counters.maxBy(_._2)
      val defaultValueProportion = (defaultValueEntry._1,
         defaultValueEntry._2*100.0/ variables.possibleValues)

      // now computes the percentages for all the values
      val proportions: Map[Double, Double] =
         counters.map(entry =>
            (entry._1, (entry._2*100)/(variables.possibleValues*1.0)))

      // gets max proportion
      val maxProportion: (Double, Double) = proportions.maxBy(_._2)
      val minProportion: (Double, Double) = proportions.minBy(_._2)
      val averageProportion: Double = proportions.values.sum/(1.0*proportions.size)

      // now perform the count of changes in the sequence of values
      val repetitions = (1 until values.length).map(index => {
         if(values(index) == values(index-1)) 1 else 0
      }).sum + 1

      // computes the percentage of the series of repeated values
      val serie: Double =
         repetitions /(1.0* variables.possibleValues)*100

      // now compose the tuple to return
      (differentValues,
         defaultValueProportion._1, defaultValueProportion._2,
         maxProportion._2, averageProportion, minProportion._2, serie)
   }

   /**
     * general method for combination
     * @param other potential to combine with
     * @return resultant potential
     */
   def combine(other: Potential): Potential = {
      // Just combine the corresponding values
      new Potential(store.combine(other.store))
   }

   /**
    * determines the potential obtained after combining
    * with another potential, discarding values computation
    * @param other potential to combine with
    * @return
    */
   def qualitativeCombine(other:Potential) : Potential = {
      // makes a potential with the domain of the result but
      // without values
      Potential(store.variables.union(other.store.variables), store.kind)
   }

   /**
     * general method for marginalization
     * @param variable variable to remove
     * @return resultant potential
     */
   def marginalize(variable: Variable): Potential = {
      // Just marginalize the values
      new Potential(store.marginalize(variable))
   }

   /**
    * determines the potential obtained after combining
    * with another potential, discarding values computation
    * @param variable variable to marginalize
    * @return
    */
   def qualitativeMarginalize(variable : Variable) : Potential = {
      Potential(store.variables.removeVariable(variable), store.kind)
   }

   /**
     * general method for normalization
     * @return result of operation
     */
   def normalize: Potential = {
      new Potential(store.normalize)
   }

   /**
    * Computes Kullback-Leibler distance between two potentials
    * It is assumed both of them are referred to the same domain
    * @param other potential to compute distance with
    */
   def KLDistance(other : Potential) : Double = {
      // considers all the possible values of this
      // and computes the distance
      (0L until store.variables.possibleValues).map(index => {
         val thisVal = store.getValue(index)
         val otherVal = other.store.getValue(index)
         thisVal*math.log(thisVal/otherVal)
      }).sum
   }

   /**
     * determines if the potential represents a distribution for
     * the last variable
     * @param variable target variable
     * @return boolean flag
     */
   def isPotentialForParentVariable(variable: Variable): Boolean = {
      variables.getVariable(variables.getSize-1) == variable
   }

   /**
     * determines if the potential contains the variable passed as
     * argument
     * @param variable variable to check
     * @return boolean flag
     */
   def isPotentialForVariable(variable: Variable): Boolean = {
      variables.contains(variable)
   }

   /**
     * checks if it is a unit potential (empty)
     * @return boolean flag
     */
   def isUnit: Boolean = {
      store == null
   }

   /**
     * General method for potential conversion
     * @param storeType desired type of potential
     * @return object with the converted potential
     */
   def convert(storeType : ValueStoreTypes.Value) : Potential = {
      Potential(variables, store.getListValues, storeType)
   }

   /**
     * toString method
     * @return string with the information of the object
     */
   override def toString: String = {
      val string1="\n--------------- Potential data -------------------\n"
      val string3=string1+store.toString
      string3
   }

   /**
    * checks two potentials comparing their values one by
    * one
    */
   override def equals(obj: Any): Boolean = {
      // assume obj has the proper type
      val pot2 = obj.asInstanceOf[Potential]

      // consider each configuration and compute the
      // number of different values
      val failures = (0L until variables.possibleValues).map(index => {
         val value1 = store.getValue(index)
         val value2 = pot2.store.getValue(index)
         Util.nearEqual(value1, value2)
      }).count(_ == false)

      // the number of failures must be 0
      failures == 0
   }

   /**
     * Gets memory size for an object of variable type
     * @return memory size estimation
     */
   def getMemorySize: Long = {
      store.getMemorySize
   }
}

/**
  * Companion object
  */
object Potential {
   /**
     * apply method acting as factory method
     * @param variableSet set of variables
     * @param valuesList list of values
     * @param kind kind of potential
     * @return object with the generated potential
     */
   def apply(variableSet: VariableSet, valuesList: List[Double],
             kind: ValueStoreType): Potential = {
      // considers the kind of storage to create
      val store = kind match {
         case ValueStoreTypes.TABLE =>
            TableStore(variableSet, valuesList.toArray)
         case ValueStoreTypes.TREE =>
            TreeStore(variableSet, valuesList.toArray)
         case ValueStoreTypes.PRUNEDTREE =>
            PrunedTreeStore(variableSet, valuesList.toArray)
         case ValueStoreTypes.VDGLSTORE =>
            VDGLStore(variableSet, valuesList.toArray)
         case ValueStoreTypes.VDGSSTORE =>
            VDGSStore(variableSet, valuesList.toArray)
         case ValueStoreTypes.IDMMSTORE =>
            IDMMStore(variableSet, valuesList.toArray)
         case ValueStoreTypes.IDSISTORE =>
            IDSIStore(variableSet, valuesList.toArray)
         case ValueStoreTypes.IDSMSTORE =>
            IDSMStore(variableSet, valuesList.toArray)
         case ValueStoreTypes.VDILISTORE =>
            VDILIStore(variableSet, valuesList.toArray)
         case ValueStoreTypes.VDILMSTORE =>
            VDILMStore(variableSet, valuesList.toArray)
         case ValueStoreTypes.VDISISTORE =>
            VDISIStore(variableSet, valuesList.toArray)
         case ValueStoreTypes.VDISMSTORE =>
            VDISMStore(variableSet, valuesList.toArray)
         case ValueStoreTypes.IDPISTORE =>
            IDPIStore(variableSet, valuesList.toArray)
         case ValueStoreTypes.IDPMSTORE =>
            IDPMStore(variableSet, valuesList.toArray)
      }

      // just creates the potential
      new Potential(store)
   }

   /**
     * second constructor
     * @param variableSet set of variables
     * @param kind kind of potential
     * @return object created
     */
   def apply(variableSet: VariableSet, kind: ValueStoreType): Potential = {
      // considers the kind of storage passed as argument
      val potential = kind match{
         case ValueStoreTypes.TABLE =>
            new Potential(TableStore(variableSet,
               //Array.fill(variableSet.possibleValues.toInt)(0.0)))
               Array.fill(1)(0.0)))
         case ValueStoreTypes.TREE =>
            new Potential(TreeStore(variableSet,
               //Array.fill(variableSet.possibleValues.toInt)(0.0)))
               Array.fill(1)(0.0)))
      }
      potential
   }

   /**
     * third constructor just defining the values directly
     * @param values values quantifying uncertainty
     * @return created object
     */
   def apply(values: ValueStore): Potential = {
      new Potential(values)
   }

   /**
     * just for unit potentials
     * @return created object
     */
   def apply: Potential = {
      new Potential(null)
   }

   /**
     * serialization method for writing an object to a file
     * @param fileName name of the file to create
     * @param potential potential to store
     */
   def writeObject(fileName: String, potential: Potential) : Unit = {
      val file=new FileOutputStream(fileName)
      val stream=new ObjectOutputStream(file)
      stream.writeObject(potential)
      stream.close()
      file.close()
   }

   /**
     * serialization method for reading objects from files
     * @param fileName name of the file to read
     * @return potential obtained from the file
     */
   def readObject(fileName: String): Potential = {
      val file=new FileInputStream(fileName)
      val stream=new ObjectInputStream(file)
      val result=stream.readObject().asInstanceOf[Potential]
      stream.close()
      file.close()
      result
   }

   /**
     * generate a random potential table
     * @param numberVariables number of variables to generate
     * @param maxCardinality maximum value for the number of states
     *                       of the variables
     * @return created object
     */
   def generateRandomPotential(numberVariables: Int,
                               maxCardinality: Int): Potential = {
      // generate the set of variables
      val variables = VariableSet.generateRandomSet(numberVariables,
                                                   maxCardinality)

      // gets the last variable
      generateRandomPotentialWithVariables(variables)
   }

   /**
     * generates a pair of potentials
     * @param numberVariables1 number of variables for the first
     *                         potential
     * @param numberVariables2 number of variables for the second
     *                         variable
     * @param maxCardinality maximum number of states for
     *                       variables
     * @return pair of created potentials
     */
   def generateRandomPotentialPair(numberVariables1 : Int,
                                   numberVariables2 : Int,
                                   maxCardinality : Int):
                                       (Potential, Potential) = {
      // generate a number of variables equal to
      // numberVariables1+numberVariables2
      val variables = VariableSet.
         generateRandomSet(numberVariables1+numberVariables2, maxCardinality)

      // selects from variables subsets of numberVariables1
      // and numberVariables2
      val variableSet1 = VariableSet(Random.shuffle(variables).
         drop(numberVariables2).toList)
      val variableSet2 = VariableSet(Random.shuffle(variables).
         drop(numberVariables1).toList)

      // generates and return both potentials
      (generateRandomPotentialWithVariables(variableSet1),
         generateRandomPotentialWithVariables(variableSet2))
   }

   /**
    * generates a pair of potentials according to the parameters
    * passed as argument
    * @param maxCardinality maximum cardinality of variables
    * @param minGlobalCardinality minimum global cardinality for all variables
    * @param maxGlobalCardinality maximum global cardinality for all variables
    * @param intervals intervals to consider between 0 and 1
    * @return
    */
   def generateRandomPotentialPair(maxCardinality : Int, minGlobalCardinality : Long,
                               maxGlobalCardinality : Long, intervals : Int): (Potential, Potential) = {
      // generate a random domain
      val domain = VariableSet.generateRandomSet(maxCardinality, minGlobalCardinality,
         maxGlobalCardinality)

      // the first potential will contain about 75% of variables
      // and the second about 50%
      val percent1 = domain.getSize*75/100
      val percent2 = domain.getSize*50/100

      // now random selects the variables for both domains
      val dom1 = VariableSet.getRandomSelection(domain, percent1)
      val dom2 = VariableSet.getRandomSelection(domain, percent2)

      // generate potentials for both domains
      val pot1 = generateRandomPotentialWithVariables(dom1, intervals, normalize = true)
      val pot2 = generateRandomPotentialWithVariables(dom2, intervals, normalize = true)

      // return both of them
      (pot1, pot2)
   }

   /**
    * generate a new random potential with the following features
    * @param maxCardinality max cardinality for variables
    * @param minGlobalCardinality min global cardinality for domain
    * @param maxGlobalCardinality max global cardinality for domain
    * @param intervals number of intervals to divide [0,1] interval
    * @param normalize boolean flag
    * @return potential
    */
   def generateRandomPotential(maxCardinality : Int, minGlobalCardinality : Long,
                              maxGlobalCardinality : Long, intervals : Int,
                              normalize : Boolean): Potential = {
      // generate the domain
      val domain = VariableSet.generateRandomSet(maxCardinality, minGlobalCardinality,
                                                maxGlobalCardinality)

      // now call the method producing the
      generateRandomPotentialWithVariables(domain, intervals, normalize)
   }

   /**
     * auxiliary method for generating potentials
     * @param variables domain variables of the potential
     * @return potential just created
     */
   private def generateRandomPotentialWithVariables(variables: VariableSet):
                                                Potential = {
      // gets the last variable
      val lastVariable = variables.getVariable(variables.size-1)

      // make a configuration without the last variable
      val conditioningVars = VariableSet(variables.slice(0, variables.size-1).toList)
      val confConditioning = Configuration(conditioningVars)

      // iterates on confConditioning states
      val iterator=confConditioning.iterator
      val valueList = iterator.flatMap(_ => {
         // generates a list of values (one for each state of the
         // last variable
         generateValues(lastVariable, doNormalization = true)
      }).toList

      // now generate the potential
      Potential(variables, valueList, ValueStoreTypes.TABLE)
   }

   /**
    * generates a random potential for a given domain of variables
    * @param variables domain of potential to generate
    * @param intervals number of intervals dividing [0,1]
    * @param normalize boolean flag
    * @return generated potential
    */
   private def generateRandomPotentialWithVariables(variables: VariableSet,
                                                   intervals: Int,
                                                   normalize : Boolean) = {
      // gets the last variable
      val lastVariable = variables.getVariable(variables.size-1)

      // make a configuration without the last variable
      val conditioningVars = VariableSet(variables.slice(0, variables.size-1).toList)
      val confConditioning = Configuration(conditioningVars)

      // gets the increment between values
      val increment = 1.0/intervals
      println("increment for values: " + increment)

      // determine the list of possible values
      val possibleValues = (0 to intervals).map(i => Util.roundNumber(increment*i)).toList

      // iterates on conditioning states
      val iterator=confConditioning.iterator
      val valueList = iterator.flatMap(_ => {
         // generates a list of values (one for each state of the
         // last variable
         generateValues(lastVariable, possibleValues, normalize)
      }).toList

      // now generates the potential
      Potential(variables, valueList, ValueStoreTypes.TABLE)
   }

   /**
    * generates a sequence of values
    * @param variable determines the number of values to generate
    * @param doNormalization boolean flag
    * @return sequence of values
    */
   private def generateValues(variable : Variable, doNormalization : Boolean) = {
      val values = (0 until variable.getNumberStates).map(_ => Random.nextInt(10)).toList

      // normalize if needed
      if(doNormalization){
         normalize(values.map(value => value.toDouble))
      }
      else{
         // values are returned after conversion to Double
         values.map(_.toDouble)
      }
   }

   /**
    * generate a sequence of values limiting selected values between a
    * discrete number of alternatives given by intervals
    * @param variable target variable
    * @param values set of available values
    * @param doNormalization boolean flag
    * @return generated list of values
    */
   def generateValues(variable : Variable, values : List[Double], doNormalization : Boolean): List[Double] = {
      // normalize if required
      if(doNormalization){
         generateNormalizedRandomValues(variable.getNumberStates, values)
      }
      else{
         // just generate values at random
         generateRandomValues(variable.getNumberStates, values)
      }
   }

   /**
    * auxiliary method for normalization a sequence of values
    * @param values list of values to normalize
    * @return
    */
   private def normalize(values : List[Double]) : List[Double] = {
      var modifiedValues = values
      var sum = values.sum
      if(sum == 0.0){
         modifiedValues = 1::values.dropRight(1)
         sum+=1
      }

      // divide by sum an return
      modifiedValues.map(value => Util.roundNumber(value / (sum * 1.0)))
   }

   /**
    * randomly generation of a certain number of values selected from
    * the list passed as second argument
    * @param counter number of values to select
    * @param values set of available values
    * @return list of generated values
    */
   private def generateRandomValues(counter : Int, values : List[Double]) : List[Double] = {
      // generate the list of values
      val indexes = (0 until counter).
         map(_ => Random.nextInt(values.length))
      indexes.map(index => values(index)).toList
   }

   /**
    * generates a sequence of values selected from a list of available ones.
    * The sequence sum up to 1
    * @param counter number of values to generate
    * @param values sequence of available values
    * @return sequence of generated values
    */
   def generateNormalizedRandomValues(counter: Int, values: List[Double]) : List[Double] = {
      val limits = ArrayBuffer.fill(counter)(1.0)
      val finalValues = ArrayBuffer.fill(counter)(0.0)

      // set a random seed
      Random.setSeed(System.currentTimeMillis())

      for(i <- 0 until counter-1){
         // generate a random number from available values
         val available = values.filter(value => value <= limits(i))
         finalValues(i) = available(Random.nextInt(available.size))

         // update limits
         limits(i+1) = Util.roundNumber(limits(i) - finalValues(i))
      }

      // the last value will be the last limit
      finalValues(counter-1) = limits(counter-1)

      // return the list of values after shuffling them
      Random.shuffle(finalValues.toList)
   }


   /**
     * Generates a certain proportion of zeros in the potential in
     * order to get potentials with specific features
     * @param potential potential to use as base for generating a new
    *                  potential with a given number of zeros
     * @param proportion proportion of indices to assign a zero value
     * @return created object
     */
   def generateZeros(potential : Potential, proportion : Double) :
                                                         Potential = {
      val generator=new Random

      // randomly selects a certain number of indices between
      // 0 and the max number of configurations for the variables
      // in the domain of the potential
      val indicesToSelect : Long =
            (potential.variables.possibleValues*proportion).toLong
      val selectedIndices =
         (1.toLong to indicesToSelect).map(_ =>
            ((Math.abs(generator.nextLong)*
            (potential.variables.possibleValues-1).toDouble)/Long.MaxValue).
            toLong)

      // for each selected index sets 0 as value
      val values=potential.store.getListValues.toArray
      selectedIndices.foreach(index => values(index.toInt) = 0)

      // creates the final potential
      Potential(potential.variables, values.toList, ValueStoreTypes.TABLE)
   }

   /**
    * Checks equality between potentials
    * @param first first potential to compare
    * @param second second potential to compare
    * @return
    */
   def equals(first : Potential, second : Potential) : Boolean = {
      // consider the values of basePotential and try
      // to find an index with different value in both
      // potentials. -1 will be the result if there is
      // equality
      val indexWithDifferences =
      (0L until first.variables.possibleValues).find(index => {
         val baseValue = first.store.getValue(index)
         val alternativeValue = second.store.getValue(index)
         val comparison = Util.nearEqual(baseValue, alternativeValue)

         // as debug info shows the values and main
         // variable if there is a difference
         if(!comparison){
            println("  difference for potential of " +
               first.mainVariable.name)
            println("  base value: " +
               baseValue+ " alternative value: " + alternativeValue)
         }

         // return the negation of comparison (true
         // of find if there are differences)
         !comparison
      }).getOrElse(-1)

      // if indexWithDiffererences is -1, return true
      indexWithDifferences == -1
   }

   /**
    * computes the global cost of the variables involved in a
    * list of potentials
    * @param potentials potentials of interest
    * @return
    */
   def getPotentialsSize(potentials : List[Potential]): Long = {
         val potsSize = potentials.
           flatMap(_.variables).distinct.map(_.getNumberStates.toLong).product
      if (potentials.size == 1) 1L
      else potsSize
   }
}
