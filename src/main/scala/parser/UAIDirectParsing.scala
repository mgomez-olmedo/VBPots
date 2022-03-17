package parser

import base.{Variable, VariableSet}
import bnet.Bnet
import potential.{Potential, ValueStoreTypes}

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Object for making a direct read of UAI files
 */
object UAIDirectParsing extends App{
   /**
    * Stores the list of variables
    */
   var variables : Map[Int, Variable] = Map()

   /**
    * List of domains for potentials
    */
   var domains : List[VariableSet] = List()

   /**
    * Complete list of potentials
    */
   var potentials : List[Potential] = List()

   /**
    * Process a file in UAI format and returns the corresponding
    * BNET
    * @param fileName name of target file
    * @return
    */
   def process(fileName : String) = {
      // open the file for reading
      val file = Source.fromFile(fileName)
      val content = file.getLines.toList
      file.close

      // first line contains mode
      println("processing UAI file: " + fileName)

      // call the auxiliary method for creating the required
      // number of variables
      variables = createVariables(content)
      println("number of variables: " + variables.size)

      // process potentials domain info
      createPotentialDomains(content)
      println("created domains of potentials " + domains.size)

      // process potential values
      createPotentials(content)
      println("processed potentials info: " + potentials.size)

      // creates the Bnet
      new Bnet("UAIModel", VariableSet(variables.values.toList),
         potentials)
   }

   /**
    * process information about variables
    * @param content string to analyze
    * @return
    */
   def createVariables(content : List[String]) = {
      // second line contains the required number of variables
      val numberVariables = content(1).toInt

      // read the line with cardinalities info
      val cardinalities: Array[Int] = content(2).split(" ").map(_.toInt)

      // creates and return the map with variables
      (0 until numberVariables).map(
         index => {
            (index, Variable(index, cardinalities(index)))
         }
      ).toMap
   }

   /**
    * process information about relations (potential domains)
    * @param content text to analyze
    */
   def createPotentialDomains(content : List[String]): Unit = {
      // read the number of relations
      val numberRelations = content(3).toInt

      // read all the potentials one to one
      domains = (0 until numberRelations).map(index => {
         // split the line into parts and discard the first one
         // containing the number of variables in the domain
         val domainInfo = content(4+index).split("\\s+").drop(1)

         // gets variables related to each string in domainInfo
         val domain = domainInfo.map(_.toInt).map(variables(_)).toList

         // creates a variableSet
         VariableSet(domain)
      }).toList
   }

   /**
    * creates all the potentials
    * @param content text to analyze
    */
   def createPotentials(content: List[String]): Unit = {
      // initialize potentials list
      potentials = List[Potential]()

      // remove empty lines
      val newContent = content.filter(line => line.nonEmpty)

      // drop lines with previous content
      val linesToDrop=4+domains.size
      var potentialContent=newContent.drop(linesToDrop)

      // read information for potentials one by one
      domains.indices.foreach(index => {
         val processedLines = createPotential(index, potentialContent)
         potentialContent=potentialContent.drop(processedLines)
      })
   }

   /**
    * creates a particular potential
    * @param index index of last variable
    * @param content text to analyze
    * @return
    */
   def createPotential(index: Int, content: List[String]) : Int = {
      // get the number of states of the last variable
      val states = domains(index).last.getNumberStates

      // compute the number of lines to read
      val lines = (domains(index).possibleValues/states).toInt

      // read all the values of the potential
      val values = (1 to lines).flatMap(index => {
         content(index).trim.split(" ").map(_.toDouble)
      }).toList

      // add the new potential to the list of potentials
      Potential(domains(index), values, ValueStoreTypes.TABLE) :: potentials
      println("   created potential with number of values: " + values.size)

      // return the number of lines to discard
      1+lines
   }

   // test the funcionality
   val filename = "./data/UAI/selected/BN_105.uaix"
   val bnet = process(filename)
   val info = bnet.getGeneralInfo
   println("number of variables: " + info._1)
   println("number of arcs: " + info._2)
   println("global number of states: " + info._3)
   println("min number of states: " + info._5)
   println("avg number of states: " + info._4)
   println("max number of states: " + info._6)

   // gets the global number of parameters
   val numberParameters = bnet.potentials.
      map(potential => potential.variables.possibleValues).sum
   println("global number of parameters: " + numberParameters)
}
