package inference

import base.{Variable, VariableSet}
import bnet.Bnet
import potential.OperatorType.OperatorType
import potential.{OperatorType, Potential}

import scala.annotation.tailrec

/**
 * Class for executing a simplified version of variable elimination
 * inference algorithm
 *
 * @constructor creates an instance of VariableElimination using as
 *              argument the name of the network to evaluate
 * @param bnet   network of interest
 * @param debugInfo flag to show if it is needed to gather information
 *                  during the evaluation
 */
class VariableElimination(val bnet : Bnet, val debugInfo: Boolean) {
   /**
    * Data members for storing the alternatives to use for
    * combination and marginalization
    */
   private var combinationAlternative = OperatorType.DEFAULT
   private var marginalizationAlternative = OperatorType.DEFAULT

   /**
    * main method of propagation. The method returns a list with
    * the posterior distributions for all the variables
    *
    * @return list with posterior potentials
    */
   def propagate: List[Potential] = {
      // for each variable determines the potentials required
      // to compute its posterior. A dictionary using entries
      // with Variable as key and a tuple with relevant and
      // irrelevant potentials for the computation of the posterior
      // of variable
      val relationsForVariables: Map[Variable, (List[Potential], List[Potential])] =
      bnet.variables.map(variable => {
         // potsForVariable contains a tupla with two lists
         // relevant - irrelevant potentials
         val potsForVariable = getRelevantPotentials(variable)

         // produce the entry for the map: variable as key
         // and tuple with relevant and irrelevant potentials
         // as value
         (variable, potsForVariable)
      }).toMap

      // now remove variables one by one
      val result: List[Potential] = relationsForVariables.map(entry => {

         // now call to getPosterior method
         val pot = getPosterior(entry._1, entry._2)

         // return pot
         pot
      }).toList

      // return result
      result
   }

   /**
    * propagate for a single variable
    *
    * @param variableName name of target variable
    * @return computed posterior for target variable
    */
   def propagate(variableName: String): Potential = {
      // gets the variable
      val variable = bnet.variables.getVariable(variableName)

      // get posterior distribution for the variable
      getPosterior(variable, getRelevantPotentials(variable))
   }

   /**
    * sets combination type for all the potentials
    * @param combType combination method to use
    * @param margType marginalization method to use
    */
   def setFunctions(combType : OperatorType, margType : OperatorType): Unit ={
      // store the alternatives
      combinationAlternative=combType
      marginalizationAlternative=margType

      // and now get sure all the potentials use the corresponding
      // functions for combination and marginalization, according
      // to the data defined in ValueStore companion object
      bnet.potentials.foreach(_.setFunctions(combType, margType))
   }

   /**
    * auxiliar method for getting the relevant and irrelevant potentials
    * for a certain variable
    *
    * @param variable variable to process obtaining the list of
    *                 relevant potentials for computation
    * @return tuple with two elements: list of relevant potentials
    *         and list of irrelevant potentials (with respect to
    *         variable)
    */
   private def getRelevantPotentials(variable: Variable):
   (List[Potential], List[Potential]) = {
      // determine relevant variables for the inference
      val relevant: List[Variable] =
         bnet.graph.relevantVariables(List(variable), List())

      // gets irrelevant potentials
      val irrelevant =
         bnet.variables.difference(new VariableSet(relevant)).variableList

      // gets potentials for irrelevant variables
      val irrelevantPots =
         irrelevant.flatMap(variable => getPotentialsForMainVariable(variable))

      // gets potentials for all the variables
      val relevantPots =
         relevant.flatMap(variable => getPotentialsForMainVariable(variable))

      // return a tuple with relevant and irrelevant
      (relevantPots, irrelevantPots)
   }

   /**
    * Gets the posterior distribution for a given variable
    *
    * @param target     variable of interest
    * @param potentials (relevant, irrelevant) potentials
    * @return posterior potential for target variable
    */
   @tailrec
   private def getPosterior(target: Variable,
                            potentials: (List[Potential], List[Potential])): Potential = {
      val relevantPotentials: List[Potential] = potentials._1
      val irrelevantPotentials: List[Potential] = potentials._2

      // with a single variable just normalize
      if (relevantPotentials.size == 1 &&
         relevantPotentials.head.variables.getSize == 1) {

         // if there is a single variable in all the potentials, just
         // combine and normalize
         val finalPot: Potential = relevantPotentials.head.normalize

         // return finalPot
         finalPot
      }
      else {
         // compute costs of removal
         val queue = computeRemovalCost(target, relevantPotentials)

         // get the best variable
         val queueInfo: VariableRemovalInfo = queue.dequeue()
         val variableToRemove = queueInfo.variable

         // remove the selected variable
         val newPots =
            removeVariable(target, variableToRemove, relevantPotentials,
               irrelevantPotentials)

         // produce a new call to getPosterior
         getPosterior(target, (newPots, irrelevantPotentials))
      }
   }

   /**
    * method to marginalize by summation a variable
    *
    * @param target               variable to remove
    * @param variableToRemove     var to remove for getting target posterior
    * @param potentials           list of potentials containing the target
    *                             variable
    * @param irrelevantPotentials list of irrelevant potentials for
    *                             generating statistics information
    * @return list of potentials obtained after removing target variable
    */
   protected def removeVariable(target: Variable, variableToRemove: Variable,
                                potentials: List[Potential],
                                irrelevantPotentials: List[Potential]): List[Potential] = {

      // gets the potentials containing target in the domain
      val potsForTarget =
         getPotentialsForVariable(variableToRemove, potentials)

      // gets the rest of potentials
      val remainingPots =
         getPotentialsWithoutVariable(variableToRemove, potentials)

      // just combine them
      val combination: Potential = if (potsForTarget.size > 1)
         potsForTarget.reduceLeft(combine(target, _, _))
      else potsForTarget.head

      // remove variables and return the result to the rest
      // of potentials
      val marginalization =
      marginalize(target, variableToRemove, combination)

      // return marginalization and the rest of potentials
      marginalization :: remainingPots
   }

   /**
    * combines two potentials
    *
    * @param target     target causing this operation
    * @param potential1 first potential to combine
    * @param potential2 second potential to combine
    * @return result of the operation
    * @note TODO:  check if protected califier can be
    *       removed
    */
   protected def combine(target: Variable, potential1: Potential,
                         potential2: Potential): Potential = {
      // check for unit potentials
      val result = if (potential1.isUnit) potential2
      else {
         if (potential2.isUnit) potential1
         else {
            //println("inference.VariableElimination::combine - pot sizes: " +
            //   potential1.variables.possibleValues + " - " + potential2.variables.possibleValues)
            //val finalDomain = potential1.variables.union(potential2.variables)
            //println("final potential: " + finalDomain.possibleValues)
            potential1.combine(potential2)
         }
      }

      // set the alternatives for combination and marginalization
      // for the potential produced as result
      result.setFunctions(combinationAlternative, marginalizationAlternative)

      // return result
      result
   }

   /**
    * method to marginalize a variable from a potential
    *
    * @param target           variable producing this operation
    * @param variableToRemove variable to remove by marginalization
    * @param potential        potential to marginalize
    * @return resultant potential
    */
   protected def marginalize(target: Variable, variableToRemove: Variable,
                             potential: Potential): Potential = {
      // check if there is a single variable or it is a unit
      // potential
      val result = if (potential.variables.variableList.size == 1 ||
         potential.isUnit) potential
      else {
         potential.marginalize(variableToRemove)
      }

      // set the alternatives for combination and marginalization
      // for the potential produced as result
      result.setFunctions(combinationAlternative, marginalizationAlternative)

      // return result
      result
   }

   /**
    * gets all the potentials having the target variable as main variable
    *
    * @param variable target variable
    * @return list of potentials
    */
   private def getPotentialsForMainVariable(variable: Variable):
                                                   List[Potential] = {
      // get a list of potentials containing the variable passed
      // as argument
      bnet.potentials.filter(potential => {
         //potential.isPotentialForParentVariable(variable)
         potential.mainVariable == variable
      }).toList
   }

   /**
    * gets all the potentials for a given variable
    *
    * @param variable target variable
    * @return lits of potentials with variable in their domains
    */
   protected def getPotentialsForVariable(variable: Variable):
                                                List[Potential] = {
      // get a list of potentials containing the variable passed
      // as argument
      bnet.potentials.filter(potential => {
         potential.isPotentialForVariable(variable)
      }).toList
   }

   /**
    * gets the list of relevant variables for a target variable
    *
    * @param variable   target variable
    * @param potentials list of potentials to check
    * @return list of potentials
    */
   protected def getPotentialsForVariable(variable: Variable,
                                          potentials: List[Potential]): List[Potential] = {
      potentials.
         filter(potential => potential.isPotentialForVariable(variable))
   }

   /**
    * From a given list of potentials gets those non related to
    * the variable
    * passed as first argument
    *
    * @param variable   target variable
    * @param potentials list of potentials to check
    * @return list of potentials without target variable
    */
   protected def getPotentialsWithoutVariable(variable: Variable,
                      potentials: List[Potential]): List[Potential] = {
      potentials.
         filter(potential => !potential.isPotentialForVariable(variable))
   }

   /**
    * Computes the cost of removal for each variable in order to
    * proceed to remove target variable. The idea is to minimize
    * the computational cost of removal
    *
    * @param target target variable
    * @param potentials potentials under consideration
    * @return priority queue obtained with the analysis
    */
   private def computeRemovalCost(target: Variable,
               potentials: List[Potential]) = {
      // the result is a priorityQueue
      val pq =
         collection.mutable.PriorityQueue[VariableRemovalInfo]()(
                              Ordering.by(VariableRemovalInfo.order))

      // gets the complete list of variables involved in potentials
      val toRemove: List[Variable] =
                  potentials.flatMap(potential => potential.variables).
                  distinct.filter(_ != target)

      // now considers each variable and and get the global size
      // of all the potentials including it
      toRemove.foreach(variable => {
         val potentialsForVariable =
            getPotentialsForVariable(variable, potentials)
         val potsSize = Potential.getPotentialsSize(potentialsForVariable)

         // add the information into the queue
         pq.enqueue(new VariableRemovalInfo(variable, potsSize))
      })

      // return pq
      pq
   }

   /**
    * Private class for storing information about cost of removing
    * variables
    *
    * @param variable target variable
    * @param size cost of removal for the variable
    */
   private case class VariableRemovalInfo(variable: Variable, size: Long)

   /**
    * Companion object
    */
   private object VariableRemovalInfo {
      /**
       * gets the order of an object of the class
       * @param info object of class VariableRemovalInfo to
       *             use in the computation of the order
       * @return inverse value of size data member of info
       *         object
       */
      def order(info: VariableRemovalInfo): Double = {
         1.0 / info.size
      }
   }

}

