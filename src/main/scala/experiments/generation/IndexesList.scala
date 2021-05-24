package experiments.generation

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

/**
 * Class for storing list of indexes for testing access function
 */
class IndexesList(val indexes : List[Long]) extends Serializable {
}

object IndexesList {
   /**
    * method for object serialization
    * @param fileName name of the file to store
    * @param indexesList object to serializa
    */
   def writeObject(fileName: String, indexesList : IndexesList) = {
      val file=new FileOutputStream(fileName)
      val stream=new ObjectOutputStream(file)
      stream.writeObject(indexesList)
      stream.close()
      file.close()
   }

   /**
    * serialization method for reading objects from files
    * @param fileName name of the file to read
    * @return potential obtained from the file
    */
   def readObject(fileName: String): IndexesList = {
      val file=new FileInputStream(fileName)
      val stream=new ObjectInputStream(file)
      val result=stream.readObject().asInstanceOf[IndexesList]
      stream.close()
      file.close()
      result
   }
}
