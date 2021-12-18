package potential.tree

import base.{Configuration, Variable, VariableSet}
import potential.TreeStore
import utils.{DataSizes, Util}

/**
 * Abstract trait for tree nodes
 */
sealed trait TreeNode extends Serializable {
   /**
    * define the number of spaces for separating node levels
    */
   final val spaces = "   "

   /**
    * gets the values stored in the tree
    *
    * @return list of values
    */
   def getValues : List[Double]

   /**
    * get the node corresponding to a given child
    *
    * @param index index of the child to access
    * @return selected child
    */
   def getChild(index: Int): TreeNode

   /**
    * restrict the tree in order to maintain the part compatible
    * with the value of the var passed as argument
    *
    * @param variable reference variable
    * @param value    value of interest for the reference variable
    * @return selected child
    */
   def restrict(variable: Variable, value: Int): TreeNode = {

      val result = this match {
         // in the case of a leaf, just return it
         case Leaf(value) => Leaf(value)

         // in the case of a internal node there are two cases
         // to consider:
         case Internal(treeVariable) =>
            // a) the internal node holds the variable passed
            // as argument. In this case, just return the
            // corresponding child
            if (treeVariable.name == variable.name) {
               TreeNode(this.getChild(value))
            }
            // b) the internal node holds another variable. In
            // this case just propagate the operation
            else {
               val result = Internal(treeVariable)
               (0 until treeVariable.getNumberStates).
                  foreach(index => {
                     result.child(index) =
                        TreeNode(this.getChild(index).
                           restrict(variable, value))
                  })

               // return result
               result
            }
      }

      // return result
      result
   }

   /**
    * Gets the size of a tree
    *
    * @return tuple with two values: leaf and internal nodes
    */
   def getNumberNodes: (Long, Long) = {
      // uses pattern matching
      this match {
         case l: Leaf => (1, 0)
         case Internal(treeVariable) =>
            (0 until treeVariable.getNumberStates).map(index => {
               val childSize = this.getChild(index).getNumberNodes
               // adds 1 to the number of internal nodes and left
               // unchanged the number of leaves
               (childSize._1, childSize._2)
            }).foldLeft((0.toLong, 1.toLong))((tuple1, tuple2) =>
               (tuple1._1 + tuple2._1, tuple1._2 + tuple2._2))
      }
   }

   /**
    * Gets memory size for an object of variable type
    *
    * @return memory size estimation
    */
   def getMemorySize: Long

   /**
    * method for printing the nodes in a tree style
    *
    * @param level level corresponding to node
    * @return string with the information of the tree
    */
   def printNode(level: Int): String
}

/**
 * Case class for leaf nodes: extends TreeNode
 *
 * @constructor creates a new object of Leaf class
 * @param value value to assign to the node
 */
case class Leaf(value: Double) extends TreeNode {
   // adds one to objects counter
   TreeNode.addObjectsCounter(false)

   /**
    * Gets the value stored in the node
    *
    * @return list of values stored in this node: just a single
    *         value
    */
   def getValues: List[Double] = {
      List(value)
   }

   /**
    * gets the child of interest. As this is the case of a
    * value node the method will return null
    *
    * @param index index of the child to access
    * @return selected child
    */
   def getChild(index: Int): TreeNode = null

   /**
    * Gets memory size for an object of variable type
    *
    * @return estimation of memory size
    */
   def getMemorySize: Long = {
      DataSizes.DOUBLE
   }

   /**
    * gets a string representing the node in a tree style
    *
    * @param level level of the node for keeping the
    *              right presentation
    * @return print node info
    */
   override def printNode(level: Int): String = {
      spaces * level + "value: " + value
   }
}

/**
 * Class for internal nodes: extends from TreeNode
 *
 * @constructor creates a new object using variable
 *              as argument
 * @param variable variable to assign to the node
 */
case class Internal(variable: Variable) extends TreeNode {
   // adds to object counter
   TreeNode.addObjectsCounter(true)

   /**
    * data member for storing the children of the internal
    * node
    */
   val child: Array[TreeNode] =
               new Array[TreeNode](variable.getNumberStates)

   /**
    * gets tha child node related to a certain index
    *
    * @param index index of interest
    * @return selected child
    */
   def getChild(index: Int): TreeNode = child(index)

   /**
    * gets the complete list of values stored by the tree
    *
    * @return list of values
    */
   def getValues: List[Double] = {
      child.indices.map(child(_)).
                           flatMap(_.getValues).toList
   }

   /**
    * prune all the children if needed. This method will
    * be called only if all the child are leaf nodes and
    * contain similar values
    *
    * @return result of prune operation
    */
   def pruneChildren: TreeNode = {
      val sumValue: Double = getValues.sum
      val average = sumValue / variable.getNumberStates

      // checks if all the values are similar to the average
      val similarValues = getValues.forall(value =>
                        Util.nearZero(Math.abs(average - value)))

      // if similarValues is true, then return single leaf
      // node with this value. In any other case, return this
      if (similarValues)
         Leaf(average)
      else this
   }

   /**
    * Gets memory size for an object of variable type
    *
    * @return memory size estimation
    */
   def getMemorySize: Long = {
      // get size due to variable
      //val variableSize = variable.getObjectSize
      val variableSize = DataSizes.VARIABLE

      // get size due to the references to the children, considering
      // the data structure used for children
      val referencesSize = child.length * DataSizes.REFERENCE

      // data structure size
      val structureSize = DataSizes.ARRAY

      // get size due to child inspecting their nodes
      val nodesSize = child.map(node => node.getMemorySize).sum

      // return the sum
      variableSize + referencesSize + nodesSize + structureSize
   }

   /**
    * gets a string representing the node in a tree style
    *
    * @param level level on node to print
    * @return string with the relevant info of the node
    */
   override def printNode(level: Int): String = {
      val output1 = spaces * level + "variable: " +
                                    variable.name + "\n"
      val output2 = child.
            map(node => node.printNode(level + 1)).mkString("\n")
      output1 + output2
   }
}

/**
 * Companion object for TreeNode
 */
object TreeNode {
   /**
    * Counter of internal nodes
    */
   private var internals: Double = 0

   /**
    * Counter of leaf nodes
    */
   private var leaves: Double = 0

   /**
    * method acting as factory method
    *
    * @param tree tree node to use for the construction
    * @return object created
    */
   def apply(tree: TreeNode): TreeNode = {
      // checks tree class
      tree match {
         case Leaf(value) =>
            // increments leaves counter
            leaves += 1

            // creates a new leaf
            Leaf(value)
         case Internal(variable) =>
            // add internal counters
            internals += 1

            // creates a new internal node
            val result = Internal(variable)
            (0 until variable.getNumberStates).foreach(index => {
               result.child(index) = TreeNode(tree.getChild(index))
            })

            // return result
            result
      }
   }

   /**
    * gets the value related to a certain index
    *
    * @param tree      tree to access
    * @param variables variables related to the domain of the
    *                  tree
    * @param index     index of the value to retrieve
    * @return value corresponding to an index
    */
   def getValue(tree: TreeNode, variables: VariableSet,
                index: Long): Double = {
      TreeStore.addGetValueCalls
      // creates a configuration for variables
      val configuration = Configuration(variables)

      // checks the structure of the node
      val result = tree match {
         case Leaf(value) => value
         case Internal(variable) =>
            val newTree =
               tree.getChild(
                  configuration.getVariableValue(variable, index))
            getValue(newTree, variables, index)
      }

      // return result
      result
   }

   /**
    * combination method for tree nodes
    *
    * @param tree1 first tree to combine
    * @param tree2 second tree to combine
    * @return result of the operation
    */
   def combine(tree1: TreeNode, tree2: TreeNode): TreeNode = {
      // considers the kind of this node
      val result = tree1 match {
         // case 1: this is a leaf node
         case Leaf(value1) =>
            tree2 match {
               // case 1-1: tree2 is a leaf node
               case Leaf(value2) =>
                  // the result is just another tree with value
                  // storing the product of value1 and value2
                  leaves += 1
                  Leaf(value1 * value2)
               // case 1-2: tree2 is a full node
               case Internal(variable) =>
                  // the result will be a new Internal node related
                  // to variable
                  internals += 1
                  val result = Internal(variable)

                  // Consider the values of variable
                  (0 until variable.getNumberStates).foreach(index => {
                     // combines tree1 with the corresponding child
                     result.child(index) =
                        combine(tree1, tree2.getChild(index))
                  })

                  // return result
                  result
            }

         // case 2: this is a internal node
         case Internal(variable1) =>
            // the result will be a new internal node for variable1
            internals += 1
            val result = Internal(variable1)

            // Consider variable values
            (0 until variable1.getNumberStates).foreach(index => {
               // Keeps on combining on tree2 restricted to the
               // current value of variable1
               result.child(index) =
                  combine(tree2.restrict(variable1, index),
                           tree1.getChild(index))
            })

            // return result
            result
      }

      // return result
      result
   }

   /**
    * Marginalize the tree over a certain variable
    *
    * @param variable variable to remove from the node
    * @param tree     tree to operate with
    * @return resultant tree node
    */
   def marginalize(variable: Variable, tree: TreeNode): TreeNode = {
      // consider the structure of tree
      val result = tree match {
         // in the case of a leaf node multiplies the value
         // by the number of states of the variable
         case Leaf(value) =>
            leaves += 1
            Leaf(value * variable.getNumberStates)
         // in the case of a internal node
         case Internal(variableNode) =>
            // in the case of both nodes are related to the
            // same variable, then jus call addChildren
            if (variable.name == variableNode.name) {
               // just add over all the children
               addChildren(tree)
            }
            else {
               // if the node is not related to the variable
               // to remove, then creates a new node for variable
               internals += 1
               val result = Internal(variableNode)

               // consider variable values
               (0 until variableNode.getNumberStates).foreach(index => {
                  result.child(index) =
                     marginalize(variable, tree.getChild(index))
               })

               // return result
               result
            }
      }

      // return result
      result
   }

   /**
    * prune method in order to collapse similar values
    *
    * @return result of the operation
    */
   def prune(tree: TreeNode): TreeNode = {
      // base case: all the child are leaf nodes
      tree match {
         case internal: Internal =>
            val allLeafs =
               internal.child.forall(child => child.isInstanceOf[Leaf])

            if (allLeafs) internal.pruneChildren
            else {
               // produce prune operation on children
               val newChildren: Array[TreeNode] =
                  internal.child.map(child => prune(child))

               // if all the children are leafs, just prune
               // it again
               val prunedChildren =
                  newChildren.forall(child => child.isInstanceOf[Leaf])

               // change the children by these new ones
               (0 until internal.variable.getNumberStates).
                  foreach(index =>
                     internal.child.update(index, newChildren(index)))

               // keeps on pruning if needed
               if (prunedChildren) internal.pruneChildren
               else internal
            }

         case _ => tree
      }
   }

   /**
    * auxiliar method for marginalization. The method is called
    * only on internal nodes
    *
    * @param tree tree to operate with
    * @return result of the operation
    */
   private def addChildren(tree: TreeNode): TreeNode = {

      // considers tree structure and acts only in the case
      // of internal node
      val result = tree match {
         case internal: Internal =>
            // now reduce children adding them
            internal.child.reduceLeft(add(_, _))
         case _ => null
      }

      // return result
      result
   }

   /**
    * auxiliar method for marginalization
    *
    * @param tree1 first tree to add
    * @param tree2 second tree to add
    * @return result of operation
    */
   private def add(tree1: TreeNode, tree2: TreeNode): TreeNode = {
      // considers tree1 structure
      val result = tree1 match {
         // case 1: tree1 is a leaf node
         case Leaf(value1) =>
            // considers tree2 structure
            tree2 match {
               // case 1-1: tree2 is a leaf node: just return a
               // new leaf node with value1 + value2 as content
               case Leaf(value2) =>
                  leaves += 1
                  Leaf(value1 + value2)

               // case 1-2: tree2 is a internal node
               case Internal(variable2) =>
                  // the result will be a new internal node for
                  // variable2
                  internals += 1
                  val result = Internal(variable2)

                  // gets each child for tree2 and add it with
                  // tree1
                  (0 until variable2.getNumberStates).foreach(index => {
                     result.child(index) = add(tree2.getChild(index), tree1)
                  })

                  // return result
                  result
            }

         // case 2: tree1 is a internal node for variable1
         case Internal(variable1) =>
            // the result will be a new Internal node for variable1
            internals += 1
            val result = Internal(variable1)

            // considers each child
            (0 until variable1.getNumberStates).foreach(index => {
               result.child(index) =
                  add(tree1.getChild(index),
                     tree2.restrict(variable1, index))
            })

            // return result
            result
      }

      // return result
      result
   }

   /**
    * Increments objects counter
    */
   def addObjectsCounter(internal: Boolean): Unit = {
      if (internal) internals += 1
      else leaves += 1
   }

   /**
    * Gets the value of objects counter
    *
    * @return objects counter
    */
   def getObjectsCounter: Double = internals + leaves

   /**
    * Reset objects counter
    */
   def resetObjectsCounter(): Unit = {
      internals = 0
      leaves = 0
   }
}