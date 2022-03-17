package experiments

import java.io.{File, PrintWriter}
import base.Variable
import bnet.Bnet
import utils.Util
import potential.ValueStoreTypes.ValueStoreType
import potential.valueBased.{VDGLStore, VDGSStore}
import potential.{Potential, TreeStore, ValueStoreTypes}

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map}

/**
  * Object for storing the configuration of the experiment
  * to perform in order to analyze a certain Bnet with several
  * kind of representations
  */
object ExperimentConfiguration{
   // stores the types of interest: only these types will be
   // considered for analysis
   var typesOfInterest = List(
      ValueStoreTypes.TABLE,
      ValueStoreTypes.TREE,
      ValueStoreTypes.PRUNEDTREE,
      //ValueStoreTypes.MAPSETGRAIN,
      ValueStoreTypes.VDGLSTORE,
      ValueStoreTypes.VDILISTORE,
      //ValueStoreTypes.MAPSETINDICESIMMUT,
      ValueStoreTypes.IDPISTORE,
      // ValueStoreTypes.IDSETIMMUT,
      ValueStoreTypes.IDMMSTORE)
}

/**
  * Class for performing the analysis on networks
  * @param folder folder with the files containing bnet data
  * @param netName name of the bnet to analyze
  * @param extension extension of file with bnet info (it
  *                  can be net or uai)
  */
class BnetAnalysis(val folder: String, val netName: String,
                   val extension : String) {
   // folder used for storing the result of the analysis
   val folderResults: String = "./analysis"
   val folderNet = "./" + folder + "/"

   // bnet created from the fileName passed as argument
   val bnet: Bnet = Bnet(netName)

   // object for storing information about representations
   val bnetsInfo: mutable.Map[ValueStoreType, BnetInfo] =
      Map[ValueStoreType, BnetInfo]()

   /**
     * Computes the information about potentials representation
     */
   def generateInfo : Unit = {
      // considers each possible representation
      ExperimentConfiguration.typesOfInterest.foreach(storeType =>
         generateInfo(storeType))

      // once generated all the information, generates the corresponding
      // result file
      generateFile
   }

   /**
     * Computes the information about a concrete type of representation
     * @param storeType
     */
   private def generateInfo(storeType: ValueStoreTypes.Value):Unit = {
      // convert the net to the required type
      val bnetConverted = if(storeType == ValueStoreTypes.TABLE) bnet
      else Bnet.convert(bnet, storeType)

      // stores the info into BnetInfo
      val netInfo = new BnetInfo(folderResults, bnet.name,
         storeType, bnetConverted, false)
      bnetsInfo.put(storeType, netInfo)

      // gets the info about memory size for all the potentials
      netInfo.potentialsInfo.map(potInfo => potInfo._2.memorySize).sum
   }

   /**
     * Generates the file with the information for analysis
     */
   private def generateFile = {
      // compose the folder where the file will be generated
      val path = folderResults + "/" +  netName + "/"
      val netFolder = new File(path)

      // creates the folder if needed
      if(!netFolder.exists()){
         netFolder.mkdir()
      }

      // compose the name of the resultFile
      val netFileName = path + netName + ".data"

      // now creates the file
      val resFile = new PrintWriter(new File(netFileName))

      // show information about the meaning of each column
      showColumnsMeaning(resFile);

      // first at all shows the info about the objects with the
      // serialization of the complete net under the representations
      // showSerializationInfo(resFile)

      // now shows information for each potential and representation
      bnet.potentials.foreach(potential => {
         // write the name of the main variable and cardinality
         // this must write columns from 1 to 10
         showPotentialGeneralInfo(resFile, potential)

         // gets the info about the representation of the potential with
         // each one of the available alternatives
         //ValueStoreTypes.values.foreach(storeType => {
         ExperimentConfiguration.typesOfInterest.foreach(storeType => {

            // show the information for concrete representations of the
            // potential
            showPotentialRepresentationInfo(resFile, potential, storeType)
         })

         // adds a new line
         resFile.println
      })

      // now show summary information about the network
      // (average information considering all the potentials)
      showNetworkSummaryInformation(resFile)

      // shows information about summary statistics for each
      // representation
      showRepresentationSummaryInformation(resFile)

      // at the end compose a line to add to latex tables used in
      // the paper
      //composeLatexTableCPTTrees(resFile)
      //composeLatexTableComparison(resFile)

      // close the writer
      resFile.close()
   }

   /**
     * Shows the header of the file. Meaning of columns:
     * general info about potential
     * 1. mainVar: main variable
     * 2. mainVarStates: number os states of main var
     * 3. potCard: potential cardinality
     * 4. valsCount: counter of different values in the potential
     * 5. defVal: default value (most repeated one)
     * 6. defValRep: repetitions of default value
     * 7. maxAvgRep: average of average proportions
     * 8. avgAvgRep: average on min proportion
     * 9. minAvgRep: percentage of sequences with repeated values with respect to
     *     the complete sequence
     * 10. seqRep: sequence of repetitions
     * Table representation
     * 11. TIndices: number of indices with stored values (all for tables)
     * 12. TVal: number of values stored (all for tables)
     * 13. TMem: memory size for table representation
     * Tree representation
     * 14.PInd: number of indices with stored values (all for trees)
     * 15. PVal: number of stored values (all for trees)
     * 16. PLeafs: number of leaf nodes
     * 17. PInternal: number of internal nodes
     * 18 PMem: memory size for tree representation
     * 19. PIndR: ratio of indices wrt tables
     * 20. PValR: ratio of values wrt tables
     * 21. PMemR: ratio of memory size
     * Pruned tree representation
     * 22.PTInd: number of indices with stored values
     * 23. PTVal: number of stored values
     * 24. PTLeafs: number of leaf nodes
     * 25. PTInternal: number of internal nodes
     * 26 PTMem: memory size for tree representation
     * 27. PTIndR: ratio of indices wrt tables
     * 28. PTValR: ratio of values wrt tables
     * 29. PTMemR: ratio of memory size
     * Map with set of grains representation (this same set of
     * columns for representation with 0 as default value)
     * 30. MSGInd: number of indices stored
     * 31. MSGVal: number of values stored
     * 32. MSGGrain: number of grains
     * 33. MSGMem: memory size
     * 34. MSGIndR: ratio of indices
     * 35. MSGValR: ratio of values
     * 36. MSGMemR: ratio of memory size
     * 37. MSGInd0D: number of indices stored
     * 38. MSGVal0D: number of values stored
     * 39. MSGGrain0D: number of grains
     * 40. MSGMem0D: memory size
     * 41. MSGIndR0D: ratio of indices
     * 42. MSGValR0D: ratio of values
     * 43. MSGMemR0D: ratio of memory size
     * Map with list of grains representation (this same set of
     * * columns for representation with 0 as default value)
     "MLGInd MLGVal MLGGrain MLGMem MLGIndR MLGValR MLGMemR ")
     * 44. MLGInd: number of indices stored
     * 45. MLGVal: number of values stored
     * 46. MLGGrain: number of grains
     * 47. MLGMem: memory size
     * 48. MLGIndR: ratio of indices
     * 49. MLGValR: ratio of values
     * 50. MLGMemR: ratio of memory size
     * 51. MLGInd0D: number of indices stored
     * 52. MLGVal0D: number of values stored
     * 53. MLGGrain0D: number of grains
     * 54. MLGMem0D: memory size
     * 55. MLGIndR0D: ratio of indices
     * 56. MLGValR0D: ratio of values
     * 57. MLGMemR0D: ratio of memory size
     * Map of list of indices representation (this same set of
     * * columns for representation with 0 as default value)
     * 58. MLIInd: number of indices stored
     * 59. MLIVal: number of values stored
     * 60. MLIMem: memory size
     * 61. MLIIndR: ratio of indices
     * 62. MLIValR: ratio of values
     * 63. MLIMemR: ratio of memory size
     * 64. MLIInd0D: number of indices stored
     * 65. MLIVal0D: number of values stored
     * 66. MLIMem0D: memory size
     * 67. MLIIndR0D: ratio of indices
     * 68. MLIValR0D: ratio of values
     * 69. MLIMemR0D: ratio of memory size
     * Map of set of indices representation
     * 70. MSIInd: number of indices stored
     * 71. MSIVal: number of values stored
     * 72. MSIMem: memory size
     * 73. MSIIndR: ratio of indices
     * 74. MSIValR: ratio of values
     * 75. MSIMemR: ratio of memory size
     * 76. MSIInd0D: number of indices stored
     * 77. MSIVal0D: number of values stored
     * 78. MSIMem0D: memory size
     * 79. MSIIndR0D: ratio of indices
     * 80. MSIValR0D: ratio of values
     * 81. MSIMemR0D: ratio of memory size
     * Array of pairs (indexPot - indexArray) and array of values
     * representation (this same set of columns for representation
     * with 0 as default value)
     * 82. AIIInd: number of indices stored
     * 83. AIIVal: number of values stored
     * 84. AIIMem: memory size
     * 85. AIIIndR: ratio of indices
     * 86. AIIValR: ratio of values
     * 87. AIIMemR: ratio of memory size
     * 88. AIIInd0D: number of indices stored
     * 89. AIIVal0D: number of values stored
     * 90. AIIMem0D: memory size
     * 91. AIIIndR0D: ratio of indices
     * 92. AIIValR0D: ratio of values
     * 93. AIIMemR0D: ratio of memory size
     * Array of sets of indices and array of values representation
     * (this same set of columns for representation with 0 as
     * default value)
     * 94. ASIIInd: number of indices stored
     * 95. ASIVal: number of values stored
     * 96. ASIMem: memory size
     * 97. ASIIIndR: ratio of indices
     * 98. ASIValR: ratio of values
     * 99. ASIMemR: ratio of memory size
     * 100. ASIIInd0D: number of indices stored
     * 101. ASIVal0D: number of values stored
     * 102. ASIMem0D: memory size
     * 103. ASIIIndR0D: ratio of indices
     * 104. ASIValR0D: ratio of values
     * 105. ASIMemR0D: ratio of memory size
     *
     * @param resFile
     */
   def showColumnsMeaning(resFile: PrintWriter) = {
      // shows the meaning of columns one by one
      // columns 1
      resFile.print("mainVar mainVarStates potCard valsCount defVal " +
         "defValRep maxAvgRep avgAvgRep minAvgRep seqRep ")
      resFile.print("TInd TVal TMem PInd PVal PLeafs PInternal PMem " +
         "PIndR PValR PMemR ")
      resFile.print("PTInd PTVal PTLeafs PTInternal PTMem PTIndR " +
         "PTValR PTMemR ")
      resFile.print("MSGInd MSGVal MSGGrain MSGMem MSGIndR MSGValR " +
         "MSGMemR ")
      resFile.print("MSGInd0D MSGVal0D MSGGrain0D MSGMem0D MSGIndR0D " +
         "MSGValR0D MSGMemR0D ")
      resFile.print("MLGInd MLGVal MLGGrain MLGMem MLGIndR MLGValR " +
         "MLGMemR ")
      resFile.print("MLGInd0D MLGVal0D MLGGrain0D MLGMem0D MLGIndR0D " +
         "MLGValR0D MLGMemR0D ")
      resFile.print("MLIInd MLIVal MLIMem MLIIndR MLIValR MLIMemR ")
      resFile.print("MLIInd0D MLIVal0D MLIMem0D MLIIndR0D MLIValR0D " +
         "MLIMemR0D ")
      resFile.print("MSIInd MSIVal MSIMem MSIIndR MSIValR MSIMemR ")
      resFile.print("MSIInd0D MSIVal0D MSIMem0D MSIIndR0D MSIValR0D " +
         "MSIMemR0D ")
      resFile.print("AIIInd AIIVal AIIMem AIIIndR AIIValR AIIMemR ")
      resFile.print("AIIInd0D AIIVal0D AIIMem0D AIIIndR0D AIIValR0D " +
         "AIIMemR0D ")
      resFile.print("ASIInd ASIVal ASIMem ASIIndR ASIValR ASIMemR ")
      resFile.println("ASIInd0D ASIVal0D ASIMem0D ASIIndR0D ASIValR0D " +
         "ASIMemR0D ")
   }

   /**
     * Shows information about size of objects representing the
     * complete network through serialization
     * @param resFile
     */
   def showSerializationInfo(resFile: PrintWriter)={
      //ValueStoreTypes.values.foreach(storeType => {
      ExperimentConfiguration.typesOfInterest.foreach(storeType => {
         // gets the entry into
         val netInfo: BnetInfo = bnetsInfo.get(storeType).getOrElse(null)
         if(netInfo != null){
            resFile.println(storeType + " " + netInfo.serializationSize)
         }
      })
   }

   /**
     * Shows general information about a potential: main variable,
     * number of states for the variable, cardinality iof the domain
     * and patterns info: default value, number of repetitions,
     * proportions (max, min and average) of repetitions of the
     * different values and number of changes in the sequence of
     * values
     * @param resFile
     * @param potential
     */
   def showPotentialGeneralInfo(resFile: PrintWriter, potential:Potential) = {
      resFile.print(potential.mainVariable.name + " " +
                     potential.mainVariable.getNumberStates +
                     " " + potential.variables.possibleValues+ " ")

      // shows information about patterns
      val patternsInfo = potential.getPatternMeasures

      // shows counter of different values, default value, number
      // of repetition of default value, max-avg-min averages of
      // repetitions (all values; 3 entries) and length of sequences
      // of repetitions
      resFile.print(f"${patternsInfo._1}%2.3f ${patternsInfo._2}%2.3f " +
         f"${patternsInfo._3}%2.3f " + f"${patternsInfo._4}%2.3f " +
         f"${patternsInfo._5}%2.3f ${patternsInfo._6}%2.3f " +
         f"${patternsInfo._7}%2.3f ")
   }

   /**
     * Show information about a particular representation of a
     * concrete potential
     *
     * @param resFile
     * @param potential
     * @param storeType
     * note
     * info for CPT: TInd TVal TMem (11, 12, 13)
     * info for T: PInd PVal PLeafs PInternal PMem PIndR PValR PMemR (14 - 21)
     * info for PT: PTInd PTVal PTLeafs PTInternal PTMem PTIndR PTValR PTMemR (22 - 29)
     * info for MSG: MSGInd MSGVal MSGGrain MSGMem MSGIndR MSGValR MSGMemR (30 - 36)
     * info for MSG0D: MSGInd0D MSGVal0D MSGGrain0D MSGMem0D MSGIndR0D
     * MSGValR0D MSGMemR0D (37-43)
     * info for MLG: MLGInd MLGVal MLGGrain MLGMem MLGIndR MLGValR MLGMemR (44-50)
     * info for MLG0D: MLGInd0D MLGVal0D MLGGrain0D MLGMem0D MLGIndR0D
     * MLGValR0D MLGMemR0D (51-57)
     * info for MLII: MLIInd MLIVal MLIMem MLIIndR MLIValR MLIMemR (58 - 63)
     * info for MLII0D: MLIInd0D MLIVal0D MLIMem0D MLIIndR0D MLIValR0D
     * MLIMemR0D (64 - 69)
     * info for MSI: MSIInd MSIVal MSIMem MSIIndR MSIValR MSIMemR (70 - 75)
     * info for MSI0D: MSIInd0D MSIVal0D MSIMem0D MSIIndR0D MSIValR0D
     * MSIMemR0D (76 - 81)
     * info for AI: AIIInd AIIVal AIIMem AIIIndR AIIValR AIIMemR (82 - 87)
     * info for AI0D: AIIInd0D AIIVal0D AIIMem0D AIIIndR0D AIIValR0D
     * AIIMemR0D (88 - 93)
     * info for ASI: ASIInd ASIVal ASIMem ASIIndR ASIValR ASIMemR (94 - 99)
     * info for ASI0D: ASIInd0D ASIVal0D ASIMem0D ASIIndR0D ASIValR0D
     * ASIMemR0D (100 - 105)
     *
     */
   def showPotentialRepresentationInfo(resFile : PrintWriter, potential : Potential,
                                       storeType : ValueStoreType) = {
      // gets the entry for the corresponding net representation
      val netInfo = bnetsInfo.get(storeType).getOrElse(null)
      if(netInfo != null){
         // get the potential for the corresponding variable
         val info: PotentialInfo = netInfo.potentialsInfo.get(potential.mainVariable).get

         // write the required info: number of indices related to
         // stored information and values stored
         resFile.print(info.sizes._1+ " " + info.sizes._2 + " ")

         // som representations require special information: for trees number of leafs
         // and number of nodes; for grains based representations, the number of grains
         if(storeType == ValueStoreTypes.PRUNEDTREE || storeType == ValueStoreTypes.TREE){
            val potTree = netInfo.net.getPotentialForVariable(potential.mainVariable)
            val nodes = potTree.store.asInstanceOf[TreeStore].root.getNumberNodes
            resFile.print(nodes._1 + " " + nodes._2 + " ")
         }
         else{
            if(storeType == ValueStoreTypes.VDGLSTORE){
               val potGrain = netInfo.net.getPotentialForVariable(potential.mainVariable)
               // get the number of grains
               val grains = potGrain.store.asInstanceOf[VDGLStore].getNumberGrains
               resFile.print(grains + " ")
            }
            else{
               if(storeType == ValueStoreTypes.VDGSSTORE){
                  val potGrain = netInfo.net.getPotentialForVariable(potential.mainVariable)
                  // get the number of grains
                  val grains = potGrain.store.asInstanceOf[VDGSStore].getNumberGrains
                  resFile.print(grains + " ")
               }
            }
         }

         // print info about memory size and serialization size
         resFile.print(info.memorySize + " ") // + info.serializationSize+ " ")

         // gets and writes the information about comparison with respect
         // to table. This will be done only if the type us not table
         if(storeType != ValueStoreTypes.TABLE){
            // get rations
            val ratios = compareRepresentations(ValueStoreTypes.TABLE, storeType, potential.mainVariable)

            // now write ratios
            resFile.print(f"${ratios._1}%2.3f ${ratios._2}%2.3f ${ratios._3}%2.3f ") // ${ratios._4}%2.3f"+ " ")
         }
      }
   }

   /**
     * Show summary information about the potentials of the network:
     * number of variables, number of arcs, total number of states,
     * average number of possible values for each domain, average
     * number of different values, average number of repetitions
     * for the most repeated values, average number of the proportion
     * of the serie of repetitions for each potential
     */
   def showNetworkSummaryInformation(resFile : PrintWriter) = {
      // gets the number of variables (i.e., number of potentials)
      // and general info about the network
      val netInfo = bnet.getGeneralInfo
      resFile.println()
      resFile.println("Number-variables: " + netInfo._1)
      resFile.println("Number-arcs: " + netInfo._2)
      resFile.println("Total-states: " + netInfo._3)
      resFile.println("Avg-states: " + netInfo._4)
      resFile.println("Min-states: " + netInfo._5)
      resFile.println("Max-states: " + netInfo._6)
      resFile.println();

      val numberVariables=netInfo._1

      // gets the complete number of parameters for all the
      // potentials
      val numberParameters = bnet.potentials.
         map(potential => potential.variables.possibleValues).sum
      resFile.println("Number-parameters: " + numberParameters)

      // gets info about average number of variables in
      // potentials
      val averageNumberVariables =
         bnet.potentials.map(potential =>
            potential.variables.size).sum/(numberVariables*1.0)
      resFile.println(f"Average-number-variables: " +
                              f"${averageNumberVariables}%2.3f")

      // gets average number of parameters per potential
      val averageNumberParameters =
         bnet.potentials.map(potential =>
            potential.variables.possibleValues).sum/numberVariables
      resFile.println(f"Average-number-parameters: " +
                              f"${averageNumberParameters}%2.3f")

      // gets info about patterns of information: default value,
      // number of repetitions of most repeated values, proportions
      // of repetitions for all the different values (max,
      // average and min) and ratio of the sequence of repetitions
      val patternsInfo: List[(Double, Double, Double, Double, Double)] =
         bnet.potentials.map(_.getPatternMeasures).
            map(info => (info._3*1.0, info._4, info._5, info._6, info._7))

      // gets info about average number of repetitions of the
      // most repeated value
      val averageRepetitions =
         patternsInfo.map(info => info._1).sum/numberVariables
      resFile.println(f"Average-repetitions-most-repeated-value: " +
         f"${averageRepetitions}%2.3f")
      val averageMaxRatio =
         patternsInfo.map(info => info._2).sum/numberVariables
      resFile.println(f"Average-max-ratio(all-values): " +
         f"${averageMaxRatio}%2.3f")
      val averageAvgRatio =
         patternsInfo.map(info => info._3).sum/numberVariables
      resFile.println(f"Average-avg-ratio(all-values): " +
         f"${averageAvgRatio}%2.3f")
      val averageMinRatio =
         patternsInfo.map(info => info._4).sum/numberVariables
      resFile.println(f"Average-min-ratio(all-values): " +
         f"${averageMinRatio}%2.3f")
      val changesRatio =
         patternsInfo.map(info => info._5).sum/numberVariables
      resFile.println(f"Average-ratio-sequence-repetitions: " +
         f"${changesRatio}%2.3f")
   }

   /**
     * Shows summary information about representations of
     * potentials using the different data structures: global
     * memory size for all the potential, average memory size
     * for potentials, ratio of memory size respect to tables
     */
   def showRepresentationSummaryInformation(resFile : PrintWriter) = {
      var tableMemorySize : Long=0L

      resFile.println()
      // consider representations one by one
      ExperimentConfiguration.typesOfInterest.foreach(storeType => {
         // shows the type of representation
         resFile.print(storeType + " ")

         // now gets all the potentials and prints info about
         // global memory size and average memory size
         val globalSize =
            bnetsInfo.get(storeType).get.potentialsInfo.
               map(info => info._2.memorySize)
         resFile.print(f"${globalSize.sum}  " +
            f"${globalSize.sum/(globalSize.size*1.0)}%2.3f  ")

         // if the type is TABLE, stores the values as reference
         if(storeType == ValueStoreTypes.TABLE){
            tableMemorySize=globalSize.sum
         }

         // now print the comparison
         resFile.println(f"${globalSize.sum/(tableMemorySize*1.0)}%2.3f")
      })
   }

   /**
     * Compose the lines with the information required for including
     * into the latex tables
     * @param resFile file to generate
     */
   def composeLatexTableCPTTrees(resFile : PrintWriter) = {
      var tableMemorySize : Long=0L

      // shows a new empty line
      resFile.println()

      // compose the line for the first table with info
      // about CPTs, trees and pruned trees
      val representations = List(ValueStoreTypes.TABLE,
               ValueStoreTypes.TREE, ValueStoreTypes.PRUNEDTREE)

      // gets the memory sizes for these representations
      val representationsInfo = representations.map(
         representation => {
            val globalSize=bnetsInfo.get(representation).get.
               potentialsInfo.map(info => info._2.memorySize).sum

            // if the type is TABLE, stores the values as reference
            if(representation == ValueStoreTypes.TABLE){
               tableMemorySize=globalSize
            }

            // compose the ratio wrt table
            val ratio = globalSize/(tableMemorySize*1.0)

            // return a tupla with global size and ratio
            (globalSize, ratio)
         }
      ).toList

      // compose a line with CPT global size, tree global size,
      // tree ratio respect CPT and pruned tree respect to CPT
      resFile.print(netName + " & ")
      resFile.print(representationsInfo(0)._1 + " & ")
      resFile.print(representationsInfo(1)._1 + " & ")
      resFile.print(f"${representationsInfo(1)._2}%2.3f" + " & ")
      resFile.print(representationsInfo(2)._1 + " & ")
      resFile.print(f"${representationsInfo(2)._2}%2.3f" + " \\\\\\hline ")
   }

   /**
     * Compose the lines with the information required for including
     * into the latex tables
     * @param resFile file to generate
     */
   def composeLatexTableComparison(resFile : PrintWriter) = {
      var tableMemorySize : Long=0L

      // shows a new empty line
      resFile.println()

      // compose the line for the first table with info
      // about CPTs, trees and pruned trees
      val representations = List(
         ValueStoreTypes.VDGSSTORE,
         ValueStoreTypes.VDGLSTORE,
         ValueStoreTypes.VDILISTORE,
         ValueStoreTypes.VDISISTORE,
         ValueStoreTypes.IDPISTORE,
         ValueStoreTypes.IDSISTORE)

      // get global size for table representation
      val tableSize = bnetsInfo.get(ValueStoreTypes.TABLE).get.
         potentialsInfo.map(info => info._2.memorySize).sum

      // gets the memory sizes for these representations
      val representationsInfo = representations.map(
         representation => {
            val globalSize=bnetsInfo.get(representation).get.
               potentialsInfo.map(info => info._2.memorySize).sum

            // compose the ratio wrt table
            val ratio = globalSize/(tableSize*1.0)

            // return a tupla with global size and ratio
            (representation, (globalSize, ratio))
         }
      ).toMap

      // compose a line the ratio for each representation
      resFile.print(netName + " & ")
      val lineString = representations.map(representation => {
         val info = representationsInfo.get(representation).get
         "%2.3f ".format(info._2)
       }).mkString("", " & ", "\\\\\\hline")

      resFile.print(lineString)
   }

   /**
     * Compose the lines with the information required for
     * including into the latex tables
     */
   def storeGlobalInfo() :
         scala.collection.immutable.Map[ValueStoreTypes.Value,Long] = {
      // compose the line for the first table with info
      // about CPTs, trees and pruned trees
      // gets the memory sizes for these representations
      ExperimentConfiguration.typesOfInterest.map(
         representation => {
            val globalSize=bnetsInfo.get(representation).get.
               potentialsInfo.map(info => info._2.memorySize).sum
            (representation,globalSize)
         }
      ).toMap
   }

   /**
     * Compares two representations obtaining ratios between
     * them. The first one is considered as the baseline
     * @param mode1 first type of interest
     * @param mode2 second type of interest
     * @param mainVar variable of interest
     */
   def compareRepresentations(mode1: ValueStoreType,
                              mode2 : ValueStoreType,
                              mainVar : Variable) = {
      val basePot = bnetsInfo.get(mode1).
                  get.potentialsInfo.get(mainVar).get
      val mode2Pot = bnetsInfo.get(mode2).
                  get.potentialsInfo.get(mainVar).get

      // computes the ratio for the number of indices stored
      val indicesRatio = 100*mode2Pot.sizes._1/(basePot.sizes._1 * 1.0)
      val valuesRatio = 100*mode2Pot.sizes._2/(basePot.sizes._2 * 1.0)
      val memRatio = 100*mode2Pot.memorySize/(basePot.memorySize * 1.0)
      val objectSizeRatio = 100*mode2Pot.serializationSize/
                                 (basePot.serializationSize * 1.0)

      // return a tuple with all these values
      (indicesRatio, valuesRatio, memRatio, objectSizeRatio)
   }

   /**
     * Shows a particular potential under a given representation
     * @param variableName name of variable of interest
     * @param storeType type of storage of interest
     */
   def showPotential(variableName:String,
                     storeType: ValueStoreTypes.Value)={
      // convert the net to the required type
      val bnetConverted = if(storeType == ValueStoreTypes.TABLE) bnet
      else Bnet.convert(bnet, storeType)

      // gets the potential
      val potential=bnetConverted.getPotentialForVariable(variableName)

      // just print the information
      println(potential)
   }
}

/**
  * Companion object for making the analysis
  */
object BnetAnalysis{
   /**
     * globalInfo for generating the global tables of results
     */
   val globalInfo : Map[String, scala.collection.immutable.Map[ValueStoreTypes.Value, Long]] =
      Map[String, scala.collection.immutable.Map[ValueStoreTypes.Value, Long]]()

   /**
     * Method for performing the analysis of the nets contained
     * in a folder
     * @param folder folder with the nets to analyze
     * @param extension extension of nets to consider
     */
   def batchAnalysis(folder: String, extension:String) = {
      val filesNet = Util.getListOfFiles(folder, extension)

      // just analyzes all of them
      filesNet.par.foreach(net => {
         println("Starting analysis of " + net)
         val netInfo = Map[ValueStoreType, Double]()
         val analyzer = new BnetAnalysis(folder, net.getName, extension)
         println("Starting call to generateInfo " + net)
         analyzer.generateInfo
         println("Ending call to generateInfo " + net)
         BnetAnalysis.globalInfo.put(net.getName, analyzer.storeGlobalInfo)
         // TODO: uncomment for generating the file
         //analyzer.generateFile
         composeLineForNet(net.getName)
      })
      println("End of analysis of net files")
      // TODO: uncomment for generating the file
      // generateGlobalFile
      generatePaperLatexTable
   }

   /**
    * generate the latex table as it appears in the paper
    * @param folder
    * @param extension
    */
   def generatePaperLatexTable  = {
      // creates the file with the data
      val texFile =
         new PrintWriter(new File("./analysis/savings.tex"))

      // compose the header of the table
      val header=composeHeader()

      // composes the lines for each net
      val netLines = BnetAnalysis.globalInfo.keys.map(net => composeLineForNet(net)).mkString

      // compose the final part of table declaration
      val finalLines = composeTableFinal()

      // sends all the content to texFile
      texFile.print(header)
      texFile.print(netLines)
      texFile.print(finalLines)

      // close file
      texFile.close()
   }

   /**
    * stores the header of the latex table
    * @return
    */
   def composeHeader() : String = {
      "\begin{table}[h!]\n" +
                   "\\centering\n" +
                   "\\begin{tabular}{|c|c|c|c|c|c|c|}\n" +
                   "\\hline\n" +
                   "network & PT & PPT & VDG & VDI & IDP & IDS \\\\\\hline\n"
   }

   /**
    * compose the final declaration of data table
    * @return
    */
   def composeTableFinal(): String = {
      "\\end{tabular}\n" +
      "\\end{table}\n"
   }

   /**
    * compose a line with the savings for a net
    * @param net
    * @return
    */
   def composeLineForNet(net: String) : String = {
      // gets the measurements for the net (map with keys
      // representations and memory sizes
      val dataForNet = BnetAnalysis.globalInfo.get(net).get

      // gets the base size: for TABLE
      val tableSize = dataForNet.get(ValueStoreTypes.TABLE).get

      // for the rest of storages compute the savings
      val savings = ExperimentConfiguration.typesOfInterest.
         filter(_ != ValueStoreTypes.TABLE).
         map(rep => {
            val memSize = dataForNet.get(rep).get

            // compute saving value
            "%07.2f".format((memSize*100.0/tableSize)-100)
         }).mkString(" & ")

      // compose and return the line
      net + " & " + savings + "\\\\\\hline\n"
   }

   /**
     * Generates a global file with the result of the analysis
     */
   def generateGlobalFile = {
      // now creates the file
      val resFile =
         new PrintWriter(new File("./analysis/globalInfo"))

      // shows the names of columns
      var line = "net "
      val columns=line::ExperimentConfiguration.typesOfInterest.map(
         representation=>representation.toString)
      resFile.println(columns.mkString(", "))

      // now gets all the names of globalInfo
      globalInfo.keys.foreach(net => {
         resFile.print(net + ", ")
         val entry: Predef.Map[ValueStoreTypes.Value, Long] =
            BnetAnalysis.globalInfo.get(net).get
         val data = ExperimentConfiguration.typesOfInterest.map(representation => {
            entry.get(representation).get
         })
         resFile.println(data.mkString(", "))
      })

      // close the file
      resFile.close()
   }

   /**
     * Single analysis for a network
     * @param netName name of the net of interest
     * @param folder folder containing the net to analyze
     * @param extension extension of the net to analyze
     */
   def singleAnalysis(netName:String, folder:String,
                      extension:String="") = {

      val analyzer = new BnetAnalysis(folder, netName, extension)
      analyzer.generateInfo
      analyzer.generateFile
      BnetAnalysis.globalInfo.put(netName, analyzer.storeGlobalInfo)

      // return the line for the net
      composeLineForNet(netName)
   }

   /**
     * Shows the particular info for a potential
     * @param netName name of the net to analyze
     * @param folder folder containing net description
     * @param variable name of the variable of interest
     * @param representation representation to analyze
     */
   def showPotential(netName:String, folder:String, variable:String,
                     representation:ValueStoreType) = {
      val analyzer=new BnetAnalysis(folder, netName, extension="net")
      analyzer.showPotential(variable, representation)
   }
}

/**
 * Simple example of generation of information for a net
 */
object ExampleAnalysys extends App{
   // just perform the analysis of interest
   print(BnetAnalysis.singleAnalysis("alarm.net","data/bnlearn/"))
}
