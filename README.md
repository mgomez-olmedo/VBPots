# VBPots

Software for testing Value-Based Potentials as alternative data structures for storing the quantitative information of Probabilistic Graphical Models (PGMs). The results of the test are included in the paper submitted to the International Journal of Intelligent Systems and titled **Value-Based Potentials Exploiting Regularity Patterns of Quantitative Information in Probabilistic Graphical Models**.

It was developed using IntelliJ as an **sbt** project and using the configuration presented in file **build.sbt**. The most straightforward way to reproduce the experiments is importing the project into IntelliJ and executing the classes from the same IDE. 

The package containing the classes required for generating the results included in the paper is titled **ijisExperiments** included in a general package (**experiments**) containing classes and tools for a wide set of experiments over VBPots. 

## Extreme case experiment

This experiment is included in section 5.5, **Example of a extreme case**. The experiment consists of generating 10 different random potentials forcing specific conditions on their values:

* only 3 values are allowed (0.0, 0.5 and 1).
* 0.0 is taken as default value.
* high percentage of repetitions for it (around 70%)

The file including the software for this experiment is stored in a specific package under **ijisExperiments** and named **extremeCases**. The file **ExtremeCaseAnalysis** include all the elements for reproducing these results. It contains a class and a executable companion object. The execution of the object perform the analysis on a single potential. At the end of the output of the software it is presented a summary of memory sizes corresponding to 1DA, PT, PPT, VDG, VDI, IDP and IDM.

## Experiments for measuring memory spaces

The experiments about memory space requirements described in section 6.2 of IJIS paper **Value-Based Potentials Exploiting Regularity Patterns of Quantitative Information in Probabilistic Graphical Models** can be reproduced using two Scala objects included in src/main/scala/experiments/ijisExperiments/memorySizes and named **BnlernMemorySizes** and **UAIMemorySizes**.

These objects are directly executable and are configured for producing the results presented in Figure 10 and Figure 11. To execute these objects it is needed to create a folder named analysis in the base folder of the project. This folder is employed for generating intermediate information produced during the analysis if the Bayesian networks used for the experiment.

## Experiments for access times

The experiments for access times are described in sections 6.3.1 and 6.3.2 and the corresponding results presented in Figures 12 and 13. It is important to note that these experiments consider a of pairs (potential, index) randomly selected. Therefore the times included in the paper may vary with new executions.

Moreover, due to the nature of the experiment, the execution times are variable due to the load of the computer. The use of Scalameter tries to minimize this effect but changes in measurements can not be avoided.

The objects for these experiments are located in the following package: **ijisExperiments/accessTimes**. The execution of these experiment requires a perliminary step for serializign the potentials of the networks to analyze. This makes the computation time for the experiment much more reduced. These code for serialization in included in two Scala objects named **SerializeBnlearnNets** and **SerializeUAINets**. These classes are directly executable via IDE and generate the complete set of serialization objects for the experiment. At the end of the serialization process there will be a new folder corresponding to the target net and and set of files with the serialization version of the network using the different representations under consideration.

After this perliminary steps, the measurements of times can be performed using two specific Scala objects. The first one is
 **NetAccessTimes**, an executable object including *main* method. It requires two arguments: name of network to analyze and number of configurations to access (experiments for paper use 10000 configurations). In our opinion, the best way to measure execution times are from outside the IDE. On possible way to do this consists of:

 1. Define a jar for this object. With IntelliJ this can be done including an artifact for the project: **File + Project Structure**. Select **Artifacts** option and include a new one with **+** icon (left top part of Artifacts section).
 2. When selecting **+** choose JAR **From modules with dependencies**. In the pop-up window titled **Create JAR from Modules** select the main class (must be NetAccessTimes). After that build the artifact with **Build + Build Artifacts** option in main menu.
 3. Move the generated jar to the base folder of the project. An launch the execution with java -jar name-of-jar and including the required arguments.
 4. These object can be used 

The second executable object extends App and is directly executable and examines the complete set of **bnlearn** networks with the default number of access to test (10000). Althought the object can be executed from the IDE, we also recommend the procedure described for the first object and make te execution from outside the IDE.

There are two objects performing the same taks on UAI networks.

## Experiments for Inference with Variable Elimination

There is a specific folder containing the object with the code for performing this part of the experimental work. As it was commented respect to access times, these experiments measure execution times and these are variable depending on the current state of the computer. The use of Scalameter makes some operations in order to minimize the changes but from execution to execution may be relevant changes in measured times.
