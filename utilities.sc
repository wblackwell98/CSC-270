import scala.io.Source
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import java.io._
import scala.annotation.tailrec

def showMe(v:Any):Unit = {
  v match {
  	case _:Corpus => {
  		for ( n <- v.asInstanceOf[Corpus].nodes) {
  			println(s"${n.urn}\t\t${n.text}")
  		}
  	}
    case _:Vector[Any] => println(s"""\n----\n${v.asInstanceOf[Vector[Any]].mkString("\n")}\n----\n""")
    case _:Iterable[Any] => println(s"""\n----\n${v.asInstanceOf[Iterable[Any]].mkString("\n")}\n----\n""")
    case _ => println(s"\n-----\n${v}\n----\n")
  }
}

def loadLibrary(fp:String):CiteLibrary = {
	val library = CiteLibrary(Source.fromFile(fp).getLines.mkString("\n"))
	library
}

def loadFile( fp: String ): Vector[String] = {
	Source.fromFile(fp).getLines.toVector
}

def saveStringVec(sv:Vector[String], filePath:String = "texts/", fileName:String = "temp.txt"):Unit = {
	val pw = new PrintWriter(new File(filePath + fileName))
	for (s <- sv){
		pw.append(s)
		pw.append("\n")
	}
	pw.close
}

def saveString(s:String, filePath:String = "texts/", fileName:String = "temp.txt"):Unit = {
	val pw = new PrintWriter(new File(filePath + fileName))
	pw.append(s)
	pw.close
}

def splitWithSplitter(text: String, puncs: String =  """[()\[\]·⸁.,; "?·!–—⸂⸃]"""): Vector[String] = {
	val regexWithSplitter = s"((?<=${puncs})|(?=${puncs}))"
	text.split(regexWithSplitter).toVector.filter(_.size > 0)
}

val punctuation: String = """[“”“‘()‘’'\[\]·_…⸁.,:; "?·!⸂⸃–—-]"""
val alphabet: String = """[A-Za-z]"""
