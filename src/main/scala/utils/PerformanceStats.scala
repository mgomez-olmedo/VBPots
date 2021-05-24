package utils

/**
 * Object for storing information about the execution
 * of the software
 */
object PerformanceStats {
   /**
    * counter for storing the number of calls to
    * getVariables method
    */
   var getVariablesCalls = 0

   /**
    * counter of time for executions of getVariables
    */
   var getVariablesTime = 0.0

   /**
    * counter of calls to getValues method
    */
   var getValueCalls = 0

   /**
    * counter of execution time for getValue method
    */
   var getValueTime = 0.0

   /**
    * counter of calls to addValue method
    */
   var addValueCalls = 0

   /**
    * execution time for addValue method
    */
   var addValueTime = 0.0

   /**
    * calls to getListValues
    */
   var getListValuesCalls = 0

   /**
    * execution time for getListValues
    */
   var getListValuesTime = 0.0

   /**
    * counter of calls to getDifferentValues
    */
   var getDifferentValuesCalls = 0

   /**
    * execution time for getDifferentValues
    */
   var getDifferentValuesTime = 0.0

   /**
    * counter of calls for getIndicesForValue
    */
   var getIndicesForValueCalls = 0

   /**
    * execution time for getIndicesForValue
    */
   var getIndicesForValueTime = 0.0

   /**
    * counter of calls to combine method
    */
   var combineCalls = 0

   /**
    * execution time of combine method
    */
   var combineTime = 0.0

   /**
    * counter of calls to marginalize method
    */
   var marginalizeCalls = 0

   /**
    * execution time of marginalize method
    */
   var marginalizeTime = 0.0

   /**
    * counter of calls to normalize method
    */
   var normalizeCalls = 0

   /**
    * execution time for normalize method
    */
   var normalizeTime = 0.0

   /**
    * increments to the counter of getVariables
    */
   def addGetVariablesCalls(): Unit = {
      getVariablesCalls = getVariablesCalls + 1
   }

   /**
    * increments the execution time of getVariables
    */
   def addGetVariablesTime(time: Double): Unit = {
      getVariablesTime = getVariablesTime + time
   }

   /**
    * increments the counter of getValue
    */
   def addGetValueCalls(): Unit = {
      getValueCalls = getValueCalls + 1
   }

   /**
    * increments the execution time of getValue
    */
   def addGetValueTime(time: Double): Unit = {
      getValueTime = getValueTime + time
   }

   /**
    * increments the counter of addValue
    */
   def addAddValueCalls(): Unit = {
      addValueCalls = addValueCalls + 1
   }

   /**
    * increments the execution time of addValue
    */
   def addAddValueTime(time: Double): Unit = {
      addValueTime = addValueTime + time
   }

   /**
    * increments the counter of getListValues
    */
   def addGetListValueCalls(): Unit = {
      getListValuesCalls = getListValuesCalls + 1
   }

   /**
    * increments execution time of getListValues
    */
   def addGetListValuesTime(time: Double): Unit = {
      getListValuesTime = getListValuesTime + time
   }

   /**
    * increments the counter of getDifferentValues
    */
   def addGetDifferentValuesCalls(): Unit = {
      getDifferentValuesCalls = getDifferentValuesCalls + 1
   }

   /**
    * increments execution time of getDifferentValues
    */
   def addGetDifferentValuesTime(time: Double): Unit = {
      getDifferentValuesTime = getDifferentValuesTime + time
   }

   /**
    * increments the counter of getIndices for value
    */
   def addGetIndicesForValueCalls(): Unit = {
      getIndicesForValueCalls = getIndicesForValueCalls + 1
   }

   /**
    * adds execution time for getIndices for value
    */
   def addGetIndicesForValueTime(time: Double): Unit = {
      getIndicesForValueTime = getIndicesForValueTime + time
   }

   /**
    * increments the counter of combine calls
    */
   def addCombineCalls(): Unit = {
      combineCalls = combineCalls + 1
   }

   /**
    * adds execution time for combine method
    */
   def addCombineTime(time: Double): Unit = {
      combineTime = combineTime + time
   }

   /**
    * increments the counter of marginalize method
    */
   def addMarginalizeCalls(): Unit = {
      marginalizeCalls = marginalizeCalls + 1
   }

   /**
    * add computation time for marginalize method
    */
   def addMarginalizeTime(time: Double): Unit = {
      marginalizeTime = marginalizeTime + time
   }

   /**
    * increments the counter for normalize method
    */
   def addNormalizeCalls(): Unit = {
      normalizeCalls = normalizeCalls + 1
   }

   /**
    * add execution time for normalize method
    */
   def addNormalizeTime(time: Double): Unit = {
      normalizeTime = normalizeTime + time
   }

   /**
     * method for printing the info about calls to methods
     */
   def printInfoCalls(): Unit = {
      println("get variable calls: " + getVariablesCalls +
                     " time: " + getVariablesTime)
      println("get value calls: " + getValueCalls +
                     " time: " + getValueTime)
      println("add value calls: " + addValueCalls +
                     " time: " + addValueTime)
      println("get list values calls: " + getListValuesCalls +
                     " time: " + getListValuesTime)
      println("get different values calls: " +
                     getDifferentValuesCalls + " time: " +
                     getDifferentValuesTime)
      println("get indices for value calls: " +
                     getIndicesForValueCalls + " time: " +
                     getIndicesForValueTime)
      println("combine calls: " + combineCalls +
                     " time: " + combineTime)
      println("marginalize calls: " + marginalizeCalls +
                     " time: " + marginalizeTime)
      println("normalize calls: " + normalizeCalls +
                     " time: " + normalizeTime)
   }

   /**
     * resets variables info
     */
   def resetVariables(): Unit = {
      getVariablesCalls = 0
      getVariablesTime = 0
      getValueCalls = 0
      getValueTime = 0
      addValueCalls = 0
      addValueTime = 0
      getListValuesCalls = 0
      getListValuesTime = 0
      getDifferentValuesCalls = 0
      getDifferentValuesTime = 0
      getIndicesForValueCalls = 0
      getIndicesForValueTime = 0
      combineCalls = 0
      combineTime = 0
      marginalizeCalls = 0
      marginalizeTime = 0
      normalizeCalls = 0
      normalizeTime = 0
   }
}

