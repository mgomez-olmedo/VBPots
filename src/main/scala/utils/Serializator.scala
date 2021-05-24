package utils

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

/**
 * object offering functions for serialization and retrieval
 */
object Serializator {
   /**
    * serialization method
    * @param fileName name of file to create
    * @param target object to serialize
    * @tparam A class of object to store
    */
   def writeObject[A](fileName : String, target : A) : Unit = {
      val file=new FileOutputStream(fileName)
      val stream=new ObjectOutputStream(file)
      stream.writeObject(target)
   }

   /**
    * read an object from a file
    * @param fileName name of file to read
    * @tparam A class of object to store
    * @return object created from file content
    */
   def readObject[A](fileName : String) : A = {
      val file=new FileInputStream(fileName)
      val stream=new ObjectInputStream(file)
      stream.readObject().asInstanceOf[A]
   }
}
