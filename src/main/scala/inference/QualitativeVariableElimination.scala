package inference

import base.Variable
import bnet.Bnet
import potential.Potential

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Class for estimating the cost of propagating for a net
 *
 * @constructor creates an instance of VariableElimination using as
 *              argument the name of the network to evaluate
 * @param netName   name of the file (in data folder) to process
 * @param extension extension for net file (net or uai)
 */
class QualitativeVariableElimination(val netName: String, val extension: String) {

   /**
    * bnet created from the fileName passed as argument
    */
   val bnet: Bnet = Bnet(netName + "." + extension)

   /**
    * list of relevant potentials for inference: it will change
    * while working with each variable
    */
   var relevantPotentials : List[Potential] = List()

   /**
    * stores the costs of removal for each variable
    */
   val costs: mutable.Map[String, List[Double]] =
                     scala.collection.mutable.Map[String, List[Double]]()

   /**
    * main method of propagation. The method returns a list with
    * the posterior distributions for all the variables
    */
   def propagate() : Unit = {
      // for each variable determines the potentials required
      // to compute its posterior. A dictionary using entries
      // with Variable as key and a tuple with relevant and
      // irrelevant potentials for the computation of the posterior
      // of variable
      val relationsForVariables: Map[Variable, List[Potential]] =
      bnet.variables.map(variable => {
         // potsForVariable contains a tupla with two lists
         // relevant - irrelevant potentials
         val potsForVariable = getConnectedVariables(variable)

         // produce the entry for the map: variable as key
         // and tuple with relevant and irrelevant potentials
         // as value
         (variable, potsForVariable)
      }).toMap

      // now remove variables one by one
      relationsForVariables.foreach(entry => {
         // sets the list of relevant potentials storing them into
         // notUsedPotentials
         relevantPotentials = entry._2
         costs.put(entry._1.name, computePotentialsSize::List())

         // proceed with computation
         getPosterior(entry._1)
      })
   }

   /**
    * propagate for a single variable
    *
    * @param variableName name of target variable
    */
   def propagate(variableName: String): Unit = {
      // gets the variable
      val variable = bnet.variables.getVariable(variableName)

      // get posterior distribution for the variable
      relevantPotentials = getBnetPotentialsForVariable(variable)
      getPosterior(variable)
   }

   /**
    * gets a string with the list of max cost for each variable
    * @return
    */
   def showMaxCosts : String = {
      var output = "Costs for " + bnet.name + ": \n"

      // for each entry get teh max value of cost
      val maxs = costs.map(entry => (entry._1, entry._2.max)).toMap[String, Double]
      // shows info for all the entries of costs
      output += maxs.mkString("\n")

      // return output
      output
   }


   def showTopMaxCosts(percentage : Double) : String = {
      var output = percentage + "% top costs for " + bnet.name + ": \n"

      // gets the number of variables corresponding to percentage
      val number = computePercentage(percentage)

      // process entries
      val selected =
         costs.map(entry => (entry._1, entry._2.max)).toSeq.sortWith(_._2 >= _._2).slice(0, number)

      // compose the final output
      output += selected.mkString("\n")

      // return output
      output
   }

   /**
    * Gets the posterior distribution for a given variable
    *
    * @param target variable of interest
    * @return posterior potential for target variable
    */
   //noinspection AccessorLikeMethodIsUnit
   @tailrec
   private def getPosterior(target: Variable) : Unit = {
      // with a single variable just normalize
      if (relevantPotentials.size == 1 &&
         relevantPotentials.head.variables.getSize == 1) {
         // if there is a single variable in all the potentials, just
         // compute the size of finalPot
         val finalPotCost = relevantPotentials.head.store.getSize._1.toDouble

         // store into the corresponding entry
         val previousList = costs(target.name)
         costs.put(target.name, finalPotCost::previousList)
      }
      else {
         // compute costs of removal
         val queue = computeRemovalCost(target)

         // get the best variable
         val queueInfo: VariableRemovalInfo = queue.dequeue()
         val variableToRemove = queueInfo.variable

         // remove the selected variable
         removeVariable(target, variableToRemove)

         // produce a new call to getPosterior
         getPosterior(target)
      }
   }

   /**
    * method to marginalize by summation a variable
    *
    * @param target               variable to remove
    * @param variableToRemove     var to remove for getting target posterior
    * @return list of potentials obtained after removing target variable
    */
   private def removeVariable(target: Variable, variableToRemove: Variable): Unit = {
      // combine relevant potentials
      val result = combinePotentials(variableToRemove, target)

      // remove variables and return the result to the rest
      // of potentials
      val marginalization = result.qualitativeMarginalize(variableToRemove)

      // add marginalization to notUSed
      addPotential(marginalization)
   }

   /**
    * combine a set of potentials in order to remove a variable
    * (first argument) during the process of computing the posterior
    * for a target variable (last argument)
    *
    * @param variableToRemove variable to remove
    * @param target target variable
    * @return
    */
   @tailrec
private def combinePotentials(variableToRemove : Variable, target : Variable) : Potential = {
      // gets the potentials containing target in the domain
      // the first element in tuple contains relevant pots and
      // the second element the irrelevant ones
      val potentials = getPotentialsForVariable(variableToRemove)

      // combine depending on the number of involved potentials
      potentials match {
         case Nil => null
         case head :: Nil =>
            // return head from notUsed
            removePotential(head)

            // return head
            head
         case head :: tail =>
            val pot1 = head
            val pot2 = tail.head

            // computes the qualitative combination of pot1 and pot2
            val result = pot1.qualitativeCombine(pot2)

            // add result to the list of available potentials
            addPotential(result)

            // remove pot1 and pot2 from used
            removePotential(pot1)
            removePotential(pot2)

            // determines the cost due to result and rest
            val cost = computePotentialsSize

            // stores this size for target
            val previousCosts = costs(target.name)
            costs.put(target.name, cost::previousCosts)

            // new call combine
            combinePotentials(variableToRemove, target)
      }
   }

   /**
    * auxiliar method for getting the relevant potentials for
    * computing the posterior for a given variable. This supposes
    * to analyze the graph and determine the set of relevant
    * variables for inference
    *
    * @param variable variable to process obtaining the list of
    *                 relevant potentials for computation
    * @return list of relevant potentials
    */
   private def getConnectedVariables(variable: Variable): List[Potential] = {
      // determine relevant variables for the inference
      val relevant: List[Variable] =
         bnet.graph.relevantVariables(List(variable), List())

      // gets potentials for all the variables
      val relevantPots =
         relevant.flatMap(variable => getBnetPotentialsForMainVariable(variable))

      // return the list of relevant potentials
      relevantPots
   }


   /**
    * gets all the potentials having the target variable as main variable
    *
    * @param variable target variable
    * @return list of potentials
    */
   private def getBnetPotentialsForMainVariable(variable: Variable):
                                                   List[Potential] = {
      // get a list of potentials containing the variable passed
      // as argument
      bnet.potentials.filter(potential => {
         //potential.isPotentialForParentVariable(variable)
         potential.mainVariable == variable
      }).toList
   }

   /**
    * gets all the potentials containing a given variable
    * in their domains
    *
    * @param variable target variable
    * @return lits of potentials with variable in their domains
    */
   private def getBnetPotentialsForVariable(variable: Variable):
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
    * @return list of potentials
    */
   private def getPotentialsForVariable(variable: Variable): List[Potential] = {
      val relevantPots = relevantPotentials.
         filter(_.isPotentialForVariable(variable))

      // return the potentials
      relevantPots
   }

   /**
    * Computes the cost of removal for each variable in order to
    * proceed to remove target variable. The idea is to minimize
    * the computational cost of removal
    *
    * @param target target variable
    * @return priority queue obtained with the analysis
    */
   private def computeRemovalCost(target: Variable) : mutable.PriorityQueue[VariableRemovalInfo] = {
      // the result is a priorityQueue
      val pq = mutable.PriorityQueue[VariableRemovalInfo]()(
                              Ordering.by(VariableRemovalInfo.order))

      // gets the complete list of variables involved in potentials
      val toRemove: List[Variable] =
                  relevantPotentials.flatMap(potential => potential.variables).
                    distinct.filter(_ != target)

      // now considers each variable and and get the global size
      // of all the potentials including it
      toRemove.foreach(variable => {
         val potentialsForVariable = getPotentialsForVariable(variable)
         val potsSize = Potential.getPotentialsSize(potentialsForVariable)

         // add the information into the queue
         pq.enqueue(new VariableRemovalInfo(variable, potsSize))
      })

      // return pq
      pq
   }

   /**
    * computes the size of the potentials passed as argument
    * @return size estimation
    */
   private def computePotentialsSize : Double = {
      val cost = relevantPotentials.map(_.store.getSize._1).sum.toDouble

      // return cost
      cost
   }

   /**
    * Removes a used potential from notUSedPotentials
    * @param usedPotential potential to remove
    */
   private def removePotential(usedPotential : Potential) : Unit = {
      relevantPotentials = relevantPotentials.filter(
         potential => !(potential.asInstanceOf[AnyRef] eq
                                          usedPotential.asInstanceOf[AnyRef]))
   }

   /**
    * adds a new potential to the list of relevant potentials
    * @param potential potential to add
    */
   private def addPotential(potential : Potential) : Unit = {
      relevantPotentials = potential :: relevantPotentials
   }

   /**
    * computes a given percentage of the number of variables
    * of the net
    * @param percentage percentage of interest
    * @return
    */
   private def computePercentage(percentage : Double) : Int = {
      (percentage*bnet.variables.size/100).toInt
   }

   /**
    * Private class for storing information about cost of removing
    * variables
    *
    * @param variable target variable
    * @param size cost of variable removal
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

