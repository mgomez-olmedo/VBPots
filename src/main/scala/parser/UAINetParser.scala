package parser

import base.Variable
import base.VariableSet
import bnet.Bnet
import potential.{Potential, ValueStoreTypes}

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Class for parsing Bayesian networks using the format of
 * files for UAI competition
 */
class UAINetParser extends JavaTokenParsers{

   /**
    * Structure of decimal values
    */
   def decimal: Parser[Double] = """\d+\.\d+""".r ^^ { _.toDouble }

   /**
    * Structure of integer values
    */
   def integer: Parser[Int] = """\d+""".r ^^ { _.toInt }

   // counters for number of variables, relations and parameters
   var numberVariables = 0
   var numberRelations = 0
   var numberParameters = 0

   // definition of several separators
   private val eol = sys.props("line.separator")
   private val eoi = """\s""".r
   private val space = " ".r
   private val tab = "\t".r
   private val separator = eoi | eol | space
   override val skipWhitespace = false

   /**
    * General method stating the structure of the net:
    * - type of model (BAYES) in our case
    * - number of variables
    * - list with number of states for each variable
    * - number of relations
    * - data of relations, one by one
    * - data of potentials, one by one
    * @return Bnet object
    */
   def component: Parser[Bnet] =
      modelType ~ variablesCounter ~ cardinalities ~ relationsCounter ~
            relations(numberRelations)  ~ potentials(numberRelations) ^^{
         // general case after extracting information of sections
         // described above
         case model ~ numberVariables ~ states ~ stringLiteral ~
               relationsList ~ potentials =>

            // creates objects for variables
            val variables: Map[Int, Variable] = (0 until numberVariables).
                     map(index => {
               // creates a variable with the required number of states and
               // stores it in a map
               (index, Variable(index, states(index)))
            }).toMap

            // creates the domains of the potentials
            val domains: List[VariableSet] = relationsList.map(relation => {
               val variablesInRelation =
                  relation.map(id => variables(id))
               VariableSet(variablesInRelation)
            })

            // creates the pairs of values and values
            val pairs: List[(VariableSet, List[Double])] = domains.zip(potentials)

            // for each pair, creates the corresponding potential
            val potentialsInNet = pairs.
               map(pair => Potential(pair._1, pair._2, ValueStoreTypes.TABLE))


            // finally creates the Bayesian net
            val net = Bnet("UAIModel",
               VariableSet(variables.values.toList), potentialsInNet)

            // return net
            net
      }

   /**
    * read de identifier of model type
    * @return string with type of model
    */
   def modelType : Parser[String] = ("MARKOV" | "BAYES") ~ separator ^^ {
      case modelType ~ sep => modelType
   }

   /**
     * read the number of variables and gives value to numberVariables
     * variable
     * @return number of variables in the model
     */
   def variablesCounter: Parser[Int] = integer ~ separator ^^ {
      case value ~ sep =>
         numberVariables=value
         numberVariables
   }

   /**
     * gets the number of relations and given value to numberRelations
     * variables
     * @return number of relations in the model
     */
   def relationsCounter: Parser[Int] = integer <~ eol ^^ {
      case value =>
         numberRelations=value
         value
   }

   /**
    * read the list of number of states for each variable
    * @return list of counters of number of states
    */
   def cardinalities: Parser[List[Int]] = rep(cardinality) <~ eol ^^ {
      case states => states
   }

   /**
    * read a single cardinality value
    * @return integer with the counter of states
    */
   def cardinality: Parser[Int] = integer <~ space ^^ {
      case value => value
   }

   /**
    * parse of relations description
    * @return list with lists of integers as elements. Each
    *         list (element) contains: number of variables
    *         involved and list of involved variables
    */
   def relations(numberRelations : Int): Parser[List[List[Int]]] =
      repN(numberRelations, relation) ~ eol ^^ {
         case allRelations ~ sep => allRelations
   }

   /**
    * parse de declaration of given relation: number of
    * involved variables, and list of variables. Variables
    * are just represented with a single number
    * @return list of variable identifiers
    */
   def relation: Parser[List[Int]] = integer ~ variables ~ eol ^^ {
      case counter ~ variables ~ sep2=> variables
   }

   /**
    * Parse the variables involved in a relation
    * @return list of variable identifiers
    */
   def variables: Parser[List[Int]] = rep1(variable) ^^ {
      case parents => parents
   }

   /**
    * parse the declaration of a single variable
    * @return variable identifier
    */
   def variable: Parser[Int] = tab ~ integer ^^ {
      case sep ~ parent => parent
   }

   /**
    * parse potentials info: as much potentials as relations
    * @return list of lists (each list contains the values
    *         of a potential)
    */
   def potentials(numberRelations : Int): Parser[List[List[Double]]] =
         repN(numberRelations, potential)

   /**
    * parse the description of a potential
    * @return list of values defining the potential
    */
   def potential: Parser[List[Double]] =
      integer  ~ eol ~ parameters  ~ eol ^^{
         case counter ~ sep1 ~ values  ~ sep2 => values
   }

   /**
    * parse the parameters of a potential
    * @return list of values
    */
   def parameters: Parser[List[Double]] = rep(line) ^^ {
      case linecontent => linecontent.flatten
   }

   /**
    * parse a single line of values
    * @return list of values in the line
    */
   def line: Parser[List[Double]] = rep1(param) ~ eol ^^ {
      case params ~ sep => params
   }

   /**
    * parse a single parameter
    * @return value
    */
   def param: Parser[Double] = space ~ decimal ^^ {
      case sep1 ~ value => value
   }

}
