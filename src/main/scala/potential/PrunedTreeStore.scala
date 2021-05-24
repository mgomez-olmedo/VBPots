package potential

import base.{Variable, VariableSet}
import potential.tree._

/**
 * Class for storing values in a tree form
 * @constructor new object using the information passed
 *              as argument
 * @param variables variables of the potential
 * @param root root node of tree
 */
class PrunedTreeStore(override val variables: VariableSet,
                      override val root: TreeNode)
                           extends TreeStore(variables, root) {
   /**
    * kind of store
    */
   override val kind: ValueStoreTypes.Value = ValueStoreTypes.PRUNEDTREE

   /**
    * method for pruning a tree store
    *
    * @return result of the operation
    */
   def prune: ValueStore = {
      new PrunedTreeStore(variables, TreeNode.prune(root))
   }

   // register available functions for marginalization
   // and combination
   registerCombinationFunction(OperatorType.DEFAULT,
      PrunedTreeStore.combineDefault)
   registerMarginalizationFunction(OperatorType.DEFAULT,
      PrunedTreeStore.marginalizeDefault)
}

/**
 * Companion object
 */
object PrunedTreeStore extends Combiner with Marginalizer{
   /**
    * Constants for the combination strategies
    */
   final val NORMAL = 1
   final val PARALLEL = 2

   /**
    * Constructor
    *
    * @param variablesInTree list of variables
    * @param values          array of values to store into the tree
    * @return created object
    */
   def apply(variablesInTree: VariableSet, values: Array[Double]):
                                                   PrunedTreeStore = {
      // Consider the list of variables
      variablesInTree.variableList match {
         // No variables: just return a leaf node
         case Nil =>
            new PrunedTreeStore(variablesInTree, Leaf(0))

         // General case
         case _ =>
            // Makes a internal node for firstVariable
            val root =
               Internal(variablesInTree.getVariable(0))
            val newTree: PrunedTreeStore =
               new PrunedTreeStore(variablesInTree, root)

            // It is needed to add the rest of variables
            // and the values
            expandTree(variablesInTree, 1,
               values, root, 0)

            // Return newTree after pruning
            newTree.prune.asInstanceOf[PrunedTreeStore]
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
                  new PrunedTreeStore(resultDomain, result)
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
   override def marginalizeDefault(valst: ValueStore, variable: Variable): ValueStore = {
      valst match {
         case TreeStore(_, root) =>
            // Gets the domain of the new potential
            val resultDomain =
               valst.variables.removeVariable(variable)

            // Gets the marginalization
            val result: TreeNode =
               TreeNode.marginalize(variable, root)

            // Makes the final potential to return
            new PrunedTreeStore(resultDomain, result)
      }
   }

   /**
    * Recursive method for expanding the tree. Auxiliary method for apply
    *
    * @param variables         variables in the tree
    * @param indexOfCurrentVar index of var under consideration
    * @param doubles           list of values
    * @param tree              node under expansion
    * @param index             index of the next value to assign
    */
   private def expandTree(variables: VariableSet, indexOfCurrentVar: Int,
                          doubles: Array[Double],
                          tree: TreeNode, index: Long): Unit = {

      // Pattern matching
      tree match {
         // Internal node
         case node: Internal =>
            // pattern matching according to the number of variables
            // to process
            val remainingVars = variables.getSize - indexOfCurrentVar

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
