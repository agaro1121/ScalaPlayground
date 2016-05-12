package scala.in.depth.types.using.types

import java.io.InputStream

/**
  * Created by Hierro on 5/8/16.
  */

trait FileLike[T] {
  def name(file : T) : String
  def isDirectory(file : T) : Boolean
  def children(directory : T) : Seq[T]
  def child(parent : T, name : String) : T
  def mkdirs(file : T) : Unit
  def content(file : T) : InputStream
  def writeContent(file : T, otherContent : InputStream) : Unit
}

/**
  * This shows how you can use type classes
  * to restrict input to a function even if
  * the args are the same type
  *
  * Notice:
  * def synchronize[F : FileLike, T : FileLike](from : F, to : T)
  *
  * This function forces the To and the From
  * */
object SynchUtil {
  def synchronize[F : FileLike, //<-- use context bounds
  T : FileLike](from : F, to : T) : Unit = {

    val fromHelper = implicitly[FileLike[F]] //<-- Look up FileLike helpers
    val toHelper = implicitly[FileLike[T]]

    def synchronizeFile(file1 : F, file2 : T) : Unit = {
      toHelper.writeContent(file2,fromHelper.content(file1)) // <-- use methods from type class
    }

    def synchronizeDirectory(dir1 : F, dir2 : T) : Unit = {
      def findFile(file : F, directory : T) : Option[T] =
        (for { file2 <- toHelper.children(directory)
               if fromHelper.name(file) == toHelper.name(file2)
        } yield file2).headOption
      for(file1 <- fromHelper.children(dir1)) {
        val file2 = findFile(file1, dir2).
          getOrElse(toHelper.child(dir2,
            fromHelper.name(file1)))
        if(fromHelper.isDirectory(file1)) {
          toHelper.mkdirs(file2)
        }
        synchronize[T,F](file2, file1)
      }
    }
    if(fromHelper.isDirectory(from)) {
      synchronizeDirectory(from,to)
    } else {
      synchronizeFile(from,to)
    } }
}


object UnRefinedSyncSystem extends App {

}
