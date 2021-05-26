package potential

import base.{Variable, VariableSet}
import potential.tree._

/**
 * Class for storing values in a tree form
 * @constructor creates a new object using the information
 *              passed as argument
 * @param variables domain of the store
 * @param root tree root node
 */
case class TreeStore(variables: VariableSet,
                     root: TreeNode) extends ValueStore
                     with Combiner with Marginalizer {
   /**
    * sets the kind of storage
    */
   override val kind: ValueStoreTypes.Value = ValueStoreTypes.TREE

   /**
    * sets behavior for default values treatment
    */
   //override val defaultValueComputerType: DefaultValueComputerType =
   //   DefaultValueComputerType.ZERO

   /**
    * Gets the value corresponding to a given index
    *
    * @param index target index
    * @return value stored in the corresponding index
    */
   override def getValue(index: Long): Double = {
      TreeStore.addGetValueCalls
      TreeNode.getValue(root, variables, index)
   }

   /**
    * Gets the complete list of values
    *
    * @return list of values
    */
   override def getListValues: List[Double] = {
      root.getValues
   }

   /**
    * Gets the list of values without repeated values
    *
    * @return list of different values
    */
   override def getDifferentValues: List[Double] = {
      getListValues.distinct
   }

   /**
    * Gets the indices containing a certain value
    *
    * @param value target value
    * @return list of indices
    */
   def getIndicesForValue(value: Double): List[Long] = {
      // gets all the values
      val values = getListValues

      // consider all the values and filter indices
      values.indices.map(index => {
         if (values(index) == value) index.toLong
         else 0.toLong
      }).filter(index => index != 0).toList
   }

   /**
    * Gets the size of the store
    *
    * @return tuple with number of indices represented, number
    *         of values stored and number of different values
    */
   override def getSize: (Long, Long, Long) = {
      // gets the number of leaves
      val storedValues = root.getValues

      // gets the number of different values
      val differentValues = storedValues.distinct

      // return a tuple with possible values, number of indices with
      // assigned values (equals to possible values), number of
      // values stored (equals to possible values) and number of
      // different values
      (storedValues.length, storedValues.length, differentValues.length)
   }

   /**
    * toString method
    *
    * @return string with object information
    */
   override def toString: String = {
      //val string1="TreeValueStore.......................\n"
      //val string2=string1+variables.toString
      //val string3=string2+"Values: \n"
      //val string4=string3+getListValues.mkString(" ")+"\n"
      //string4
      printTree
   }

   def printTree: String = {
      var output = "TreeValueStore.......................\n"
      output = output + " Type: " + kind + "\n"
      output = output + variables.toString
      output = output + "Values: \n"
      output = output + root.printNode(0) + "\n"
      val info = getSize
      val leaves = root.getNumberNodes._1
      val internals = root.getNumberNodes._2
      output = output + "internal nodes: " + internals + " leaves: " +
         leaves + "\n"
      output = output + " indices: " + info._1 + " values: " +
         info._2 + " diff. values: " + info._3 + "\n"
      output = output + "memory size: " + getMemorySize + "\n"
      output
   }

   /**
    * Gets memory size for an object of variable type
    *
    * @return memory size estimation
    */
   def getMemorySize: Long = {
      // gets sizes due to domain
      val variableSize = variables.getMemorySize

      // gets sizes due to the tree
      val valuesSize = root.getMemorySize

      // return the sum
      variableSize + valuesSize
   }

   // register available functions for marginalization
   // and combination
   registerCombinationFunction(OperatorType.DEFAULT,
      TreeStore.combineDefault)
   registerMarginalizationFunction(OperatorType.DEFAULT,
      TreeStore.marginalizeDefault)
}

/**
 * Companion object
 */
object TreeStore extends Combiner with Marginalizer {
   var getValueCalls = 0

   def addGetValueCalls = {
      getValueCalls+=1
   }

   def getGetValueCalls = getValueCalls

   /**
    * Constructor
    *
    * @param variablesInTree list of variables
    * @param values          array of values to store into the
    *                        tree
    * @return created object
    */
   def apply(variablesInTree: VariableSet, values: Array[Double]):
                                                   TreeStore = {
      // Consider the list of variables
      variablesInTree.variableList match {
         // No variables: just return a leaf node
         case Nil => TreeStore(variablesInTree, Leaf(0))

         // General case
         case _ =>
            // Makes a internal node for firstVariable
            val root =
               Internal(variablesInTree.getVariable(0))
            val newTree: TreeStore =
               TreeStore(variablesInTree, root)

            // It is needed to add the rest of variables and
            // the values
            expandTree(variablesInTree, 1,
                        values, root, 0)

            // Return newTree
            newTree
      }
   }

   /**
    * Combination method
    *
    * @param valst1 first potential to combine
    * @param valst2 second potential to combine
    * @return result of combination
    */
   override def combineDefault(valst1: ValueStore, valst2: ValueStore): ValueStore = {
      //root.combine(other.root)
      valst1 match {
         case TreeStore(_, root) =>

            valst2 match {
               case TreeStore(otherVariables, otherRoot) =>
                  // Gets the domain of the new potential
                  val resultDomain =
                     valst1.variables.union(otherVariables)

                  // Gets the combination
                  val result: TreeNode =
                     TreeNode.combine(root, otherRoot)

                  // Makes the final potential to return
                  TreeStore(resultDomain, result)
            }
      }
   }

   /**
    * Marginalization method
    *
    * @param valst    potential to marginalize
    * @param variable variable to remove
    * @return result of marginalization
    */
   override def marginalizeDefault(valst: ValueStore, variable: Variable):
                                                ValueStore = {
      valst match {
         case TreeStore(_, root) =>
            // Gets the domain of the new potential
            val resultDomain =
               valst.variables.removeVariable(variable)

            // Gets the marginalization
            val result: TreeNode =
               TreeNode.marginalize(variable, root)

            // Makes the final potential to return
            TreeStore(resultDomain, result)
      }
   }

   /**
    * Recursive method for expanding the tree. Auxiliary method for
    * apply
    *
    * @param variables         variables in the tree
    * @param indexOfCurrentVar index of var under consideration
    * @param doubles           list of values
    * @param tree              node under expansion
    * @param index             index of the next value to assign
    */
   private def expandTree(variables: VariableSet, indexOfCurrentVar: Int,
                          doubles: Array[Double], tree: TreeNode,
                          index: Long): Unit = {

      // Pattern matching
      tree match {
         // Internal node
         case node: Internal =>
            // pattern matching according to the number of
            // variables to process
            val remainingVars =
                     variables.getSize - indexOfCurrentVar

            remainingVars match {
               case 0 =>
                  // Only 1 remaining var
                  (0 until node.variable.getNumberStates).
                     foreach(state => {
                        node.child(state) =
                           Leaf(doubles(index.toInt + state))
                     })

               case _ =>
                  // More than one
                  (0 until node.variable.getNumberStates).
                     foreach(state => {
                        // adds one to object counter
                        val newTree =
                           Internal(variables.getVariable(indexOfCurrentVar))
                        node.child(state) = newTree
                        expandTree(variables, indexOfCurrentVar + 1,
                           doubles, newTree,
                           index + state * variables.weights(indexOfCurrentVar - 1))
                  })
            }

         case _ =>
      }
   }
}
