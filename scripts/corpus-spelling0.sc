import scala.io.Source
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import java.io._
import scala.annotation.tailrec
import java.util.Calendar

:load scripts/corpus-tokenize2.sc


val spellCheckSplitters: String =  """[()\[\]·⸁.,…:_; "?·!–—⸂⸃-]"""

val lib: CiteLibrary = loadLibrary("text/arist_politics.cex")

val standardDict: Vector[String] = loadFile("validation-data/SCOWL-wl/words.txt")
val userDict: Vector[String] = loadFile("validation-data/userDictionary.txt")
val words: Vector[String] = standardDict ++ userDict

val tr: TextRepository = lib.textRepository.get

val cat: Catalog = tr.catalog

val corp: Corpus = tr.corpus

/* For this, we just want the words */

val justTheTextParts: Vector[String] = corp.nodes.map( _.text )
val oneBigString: String = justTheTextParts.mkString(" ")
val splitIntoWords: Vector[String] = {
	val firstSplit: Vector[String] = oneBigString.split(spellCheckSplitters).toVector
	val noEmpties: Vector[String] = firstSplit.filter( _.size > 0 )
	noEmpties.distinct // Why do we ask for `.distinct`?
}

val badWordVec: Vector[String] = {
	splitIntoWords.filter( w => { 
				val inDict: Boolean = standardDict.contains(w)
				val inUserDict: Boolean = userDict.contains(w)
				( inDict | inUserDict ) == false 
	})
}