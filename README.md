# VBPots-V3

Software for testing Value-Based Potentials as alternative data structures for storing the quantitative information of Probabilistic Graphical Models (PGMs).

It was developed using IntelliJ as an **sbt** project and using the configuration presented in file **build.sbt**. The most straightforward way to reproduce the experiments is importing the project into IntelliJ and executing the classes from the same IDE. 

## Experiments for measuring memory spaces

The experiments about memory space requirements described in section 6.2 of IJIS paper **Value-Based Potentials Exploiting Regularity Patterns of Quantitative Information in Probabilistic Graphical Models** can be reproduced using two Scala objects included in src/main/scala/experiments/ijisExperiments/memorySizes and named **BnlernMemorySizes** and **UAIMemorySizes**.

These objects are directly executable and are configured for producing the results presented in Figure 10 and Figure 11. To execute these objects it is needed to create a folder named analysis in the base folder of the project. This folder is employed for generating intermediate information produced during the analysis if the Bayesian networks used for the experiment.

## Experiments for access times

The experiments are described in sections 6.3.1 and 6.3.2 and the results presented in Figures 12 and 13. 

