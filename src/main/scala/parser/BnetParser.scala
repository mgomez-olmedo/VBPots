package parser

import bnet._

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Class for parsing Bayesian networks in net (bnlearn) format
 *
 */
class BnetParser extends JavaTokenParsers {
   /**
    * Main method for parsing net information: net name, nodes
    * information and potentials data
    *
    * @return
    */
   def component: Parser[Bnet] = net ~ nodes ~ potentials ^^ {
      // Uses pattern matching: specifies the three sections of
      // the format: net info, nodes info and potentials info. The
      // result of parsing these sections will ne stored into this
      // three components;: net (string), nodes (list with tuples
      // composed by var name and state names) and potentials (list
      // tuples with:
      // a) tuple with main variable and parent variables
      // b) list of double values quantifying the potential
      case net ~ nodes ~ potentials =>
         // remove quotes form var and state names
         val filteredNodes = nodes.map(entry => {
            val varName = removeCharacter(entry._1, '"')
            val states = entry._2.
               map(stateName => removeCharacter(stateName, '"'))
            // for each tuple remove quotes and creates a new
            // tuple after this transformation
            (varName, states)
         })

         // just creates the net with the information parsed
         // from the file
         Bnet(net, filteredNodes, potentials)
   }

   /**
    * Method for parsing net information: net reserved word,
    * "{", netName and "}"
    *
    * @return string with net name
    */
   def net: Parser[String] = "net" ~ "{" ~> netName <~ "}"

   /**
    * Method for parsing net name (some times the name is
    * omitted in bnet files)
    *
    * @return string with net name
    */
   def netName: Parser[String] = opt(ident) ^^ {
      case None => ""
      case Some(ident) => ident
   }

   /**
    * Method for parsing nodes information.
    *
    * @return list of tuples node name (string) - node states
    *         (list of string). The definition of node is made
    *         following the structure stated in nodeDefinition
    */
   def nodes: Parser[List[(String, List[String])]] = rep(nodeDefinition)

   /**
    * Method for parsing a node information. The definition is
    * composed of nodeId and nodeStates
    *
    * @return tuple node name (string) - node states (list of string)
    */
   def nodeDefinition: Parser[(String, List[String])] =
      nodeID ~ nodeStates ^^ {
         case variableName ~ variableStates => (variableName, variableStates)
   }

   /**
    * Method for parsing the identifier of a node. The format uses
    * "node" reserved word and the name for the node
    *
    * @return string with node identifier (name)
    */
   def nodeID: Parser[String] = "node" ~> ident

   /**
    * Method for parsing state nodes. The format uses keys
    * for separating the description of the states
    *
    * @return lista of names (string)
    */
   def nodeStates: Parser[List[String]] = "{" ~> states <~ "}"

   /**
    * Auxiliar method for parsing states information. The format
    * uses "states" reserved key, equals sign and the names of
    * states between brackets
    *
    * @return list of names (string)
    */
   def states: Parser[List[String]] =
      "states = (" ~> rep(stringLiteral) <~ ");"

   /**
    * Method for parsing potentials information: all of them
    * following the same syntax described by potential
    *
    * @return list of tuplas ( (node name - node states (list)),
    *         probability values (list of double) )
    */
   def potentials: Parser[List[((String, List[String]), List[Double])]] =
      rep(potential)

   /**
    * Auxiliar method for parsing each potential info
    *
    * @return tupla with ( (node name - parent nodes (list)) and
    *         probability values (list of double) )
    */
   def potential: Parser[((String, List[String]), List[Double])] =
      potentialDomain ~ potentialData ^^ {
         case variablesTuple ~ valuesList => (variablesTuple, valuesList)
   }

   /**
    * Auxiliar method for parsing potential info: domain. The format
    * specifies "potential" keyword, and specification of variables
    * composing the domain: mainVar, "|" and parent nodes
    *
    * @return tupla with main variable and list of parent variables
    */
   def potentialDomain: Parser[(String, List[String])] =
      "potential (" ~> variables <~ ")"

   /**
    * Auxiliar method for parsing variables defining a potential
    * domain: name of main variable and, in the case of conditional
    * potentials, "|" and names of conditioning variables.
    *
    * @return tupla with main variable - parent variables
    */
   def variables: Parser[(String, List[String])] =
      ident ~ opt("|" ~> rep(ident)) ^^ {
         // general case: conditional potential
         case variableName ~ Some(parents) => (variableName, parents)
         // marginal potential
         case variableName ~ None => (variableName, List[String]())
   }

   /**
    * Auxiliar method for parsing potential info: data. The
    * quantitative information is enclosed by keys
    *
    * @return list of probability values
    */
   def potentialData: Parser[List[Double]] = "{" ~> data <~ "}"

   /**
    * Auxiliar method for parsing information about values in
    * potentials. Format: "data" reserved word, "=" and values
    * enclosed by brackets
    *
    * @return list of double values
    */
   def data: Parser[List[Double]] = "data = (" ~> values <~ ")" ~ ";"

   /**
    * Auxiliar method for parsing information about values in
    * potentials (just the values). The format of values is
    * specified by valuesPattern
    *
    * @return list of probability values
    */
   def values: Parser[List[Double]] = rep(valuesPattern) ^^ {
      list => list.flatten
   }

   /**
    * Auxiliar method defining values pattern: a single value
    * or another group of values enclosed by brackets
    *
    * @return list of probability values
    */
   def valuesPattern: Parser[List[Double]] =
      probValue | "(" ~> values <~ ")"

   /**
    * Auxiliar method for parsing probability values
    *
    * @return list of probability values
    */
   def probValue: Parser[List[Double]] = floatingPointNumber ^^ {
      value => value.toDouble :: List[Double]()
   }

   /**
    * Removes the occurrences of character for the string passed as
    * second argument
    *
    * @param target target string
    * @param character caharacter of interest
    * @return final string
    */
   def removeCharacter(target: String, character: Char): String = {
      target.replace(character.toString, "")
   }
}
