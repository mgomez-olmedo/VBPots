package graph

import base.Variable
import bnet.Bnet

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
 * Class for representing a graph related to a Bayesian
 * network. The class implements Bayes-Ball algorithm
 * @constructor produces a new object using the data passed
 *              as argument
 * @param name name of the related network
 * @param nodes list of nodes
 * @param mapVariableNode dictionary for storing the relation
 *                        between variables in the network and
 *                        nodes in the graph. This map is created
 *                        by apply method in companion object
 */
class Graph(val name: String, val nodes: List[Node],
            val mapVariableNode: Map[Variable, Node]) extends Serializable {

   /**
    * detect relevant variables for a given query
    *
    * @param j target variables
    * @param k observed variables
    * @return list of relevant variables for the target
    */
   def relevantVariables(j: List[Variable], k: List[Variable]): List[Variable] = {
      // first at all reset all ticks
      nodes.foreach(node => node.resetFlags())

      // all the nodes in j are marked as belonging to this set
      val nodesInJ = nodes.filter(node => j.contains(node.variable))
      nodesInJ.foreach(node => node.inJ = true)

      // the same for k: mark nodes in k modifying inK data member
      nodes.filter(node => k.contains(node.variable)).
         foreach(node => node.inK = true)

      // starts the bouncing procedure
      bounce(nodesInJ)

      // now gets all the nodes marked on top
      nodes.filter(node => node.top).map(node => node.variable)
   }

   /**
    * Method performing ball movements
    */
   @tailrec
   private def bounce(toVisit: List[Node]): Unit = {
      if (toVisit.nonEmpty) {
         // gets the first element
         val node = toVisit.head

         // deletes the node from toVisit list
         val restToVisit = toVisit.tail

         // call the method of ball propagation depending
         // on fromParent flag if needed
         val moreToVisit: List[Node] =
         if (node.fromParent) {
            receiveBallFromParent(node, restToVisit)
         }
         else {
            receiveBallFromChild(node, restToVisit)
         }

         // makes a new call to bounce
         bounce(moreToVisit)
      }
   }

   /**
    * auxiliar method for Bayes Ball algorithm. It reproduces the action
    * of receiving a ball from a child
    *
    * @param actual node under analysis
    * @param toVisit list of nodes to visit
    * @return list of nodes to be visited as a result of the
    *         ball movement
    */
   private def receiveBallFromChild(actual: Node, toVisit: List[Node]):
                                             List[Node] = {
      // start a result variable with toVisit
      var result: List[Node] = toVisit

      // mark as visited
      actual.visited = true

      // if it is not in K
      if (!actual.inK) {
         // if not marked on top, gets the new list of nodes
         // to visit
         if (!actual.top) {
            // activate top mark
            actual.top = true

            // mark all parent nodes in order to be visited from
            // child
            actual.parents.foreach(parentVariable => {
               // gets the node for parent (a Variable)
               val parentNode = getNodeForVariable(parentVariable)

               // include parentNode if needed
               result = if (parentNode.scheduled) result else {
                  // mark it as scheduled
                  parentNode.scheduled = true
                  parentNode.fromParent = false
                  result ::: List(parentNode)
               }
            })
         }

         // if not marked on bottom
         if (!actual.down) {
            // mark it
            actual.down = true

            // mark all children nodes in prder to be visited from
            // parent
            actual.children.foreach(childVariable => {
               val childNode = getNodeForVariable(childVariable)

               // include childNode if needed
               result = if (childNode.scheduled) result else {
                  childNode.scheduled = true
                  childNode.fromParent = true
                  result ::: List(childNode)
               }
            })
         }
      }

      // return the list of nodes to be visited
      result
   }

   /**
    * auxiliar method for Bayes Ball algorithm. It reproduces the
    * operations required for receiving a ball from a parent node
    *
    * @param actual node under analysis
    * @param toVisit list of nodes to visit
    * @return list of nodes to be visited as a result of the
    *         ball movement
    */
   private def receiveBallFromParent(actual: Node, toVisit: List[Node]):
   List[Node] = {
      // variable result
      var result = toVisit

      // mark as visited
      actual.visited = true

      // if belongs to k then...
      if (actual.inK) {
         // check if marked on top
         if (!actual.top) {
            // sets the flag
            actual.top = true

            // get parent nodes and includes them into the list
            // of nodes to be visited
            actual.parents.foreach(parentVariable => {
               // gets the corresponding parent node
               val parentNode = getNodeForVariable(parentVariable)

               // add the node to result if needed
               result = if (parentNode.scheduled) result
               else {
                  // shows that it must be visited from child
                  parentNode.fromParent = false
                  parentNode.scheduled = true
                  result ::: List(parentNode)
               }
            })
         }
      }
      else {
         // check down mark
         if (!actual.down) {
            // sets the mark
            actual.down = true

            // mark children nodes in order to be visted from parent
            actual.children.foreach(childVariable => {
               val childrenNode = getNodeForVariable(childVariable)

               // return childrenNode if needed
               result = if (childrenNode.scheduled) result
               else {
                  // mark the visit from parent
                  childrenNode.fromParent = true
                  childrenNode.scheduled = true
                  result ::: List(childrenNode)
               }
            })
         }

      }

      // return result
      result
   }

   /**
    * private method for getting the node corresponding to a variable
    *
    * @param variable target variable
    * @return node related to variable argument
    */
   private def getNodeForVariable(variable: Variable): Node =
      mapVariableNode(variable)

   /**
    * toString method: offers a string containing information
    * about the graph: that it, the state of its nodes
    *
    * @return
    */
   override def toString: String = {
      nodes.map(node => node.toString).mkString("\n")
   }
}

/**
 * Class for representing a node of the graph (internal class)
 * @constructor creates an object representing a node of the
 *              graph. It used the information passed as argument:
 * @param variable variable related to the node
 * @param parents list of parent nodes
 * @param children list of children nodes
 */
class Node(val variable: Variable, val parents: List[Variable],
           val children: List[Variable]) extends Serializable {
   /**
    * Data members to define the state of the node (info explained
    * in Bayes-Ball algorithm)
    */
   var inJ: Boolean = false
   var inK: Boolean = false
   var top: Boolean = false
   var down: Boolean = false
   var visited: Boolean = false
   var fromParent = false
   var scheduled = false

   /**
    * toString method: it offers a string containing the relevant
    * information of a node
    *
    * @return
    */
   override def toString: String = {
      val output1: String =
         "------------------------------------------------------\n"
      val output2: String = output1 + "Node for variable: " +
         variable.name + "\n"
      val output3 = output2 + "Parents: " + parents + "\n"
      val output4 = output3 + "Children: " + children + "\n"
      val output5 = output4 + "top: " + top + " down: " + down +
         " visited: " + visited + "\n"
      val output6 = output5 + "inJ: " + inJ + " inK: " + inK +
         " fromParent: " + visited + "\n"
      val output7 =
         output6 + "-------------------------------------------------------\n"
      // return the final string
      output7
   }

   /**
    * Method for resetting node flags
    */
   def resetFlags(): Unit = {
      inJ = false
      inK = false
      top = false
      down = false
      visited = false
      fromParent = false
      scheduled = false
   }
}

/**
 * Companion object
 */
object Graph {
   /**
    * Generates a graph from potentials of a Bayesian network
    *
    * @return created object
    */
   def apply(bnet: Bnet): Graph = {
      // create nodes for all the variables
      val nodes: List[Node] = bnet.variables.map(variable => {
         // get the potential having this variable as the main one
         val potentialForVariable = bnet.potentials.
            find(potential => potential.mainVariable == variable).get

         // gets the variables acting as parents
         val parents: List[Variable] =
            potentialForVariable.conditioningVars

         // gets variables related to potentials where variable
         // belongs to conditioningVars
         val children: List[Variable] = bnet.potentials.
            filter(potential =>
               potential.conditioningVars.contains(variable)).
               map(potential => potential.mainVariable)

         // create the node
         new Node(variable, parents, children)
      }).toList

      // for each node creates a pair Variable - Node
      val mapVariableNode: Map[Variable, Node] =
         nodes.map(node => (node.variable, node)).toMap

      // creates and return the graph
      new Graph(bnet.name, nodes, mapVariableNode)
   }
}