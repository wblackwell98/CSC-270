import scala.io.Source
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import java.io._
import scala.annotation.tailrec
import java.util.Calendar

:load utilities.sc

val lib: CiteLibrary = loadLibrary("text/arist_politics.cex")

val tr: TextRepository = lib.textRepository.get

val cat: Catalog = tr.catalog

val corp: Corpus = tr.corpus

/* Lets divide the text into Sections.

	 My work is Book/Section/Paragraph

*/

val dropCitationLevels: Int = 1

/* We want, instead of one Corpus, a Vector[Corpus] 
	 where each Corpus has contains one Section of the text
*/

val chunks: Vector[Corpus] = {
		val urns: Vector[CtsUrn] = {
			corp.urns.map( _.collapsePassageBy(dropCitationLevels)).distinct
		}	
		val corps: Vector[Corpus] = {
			urns.map( u => {
				corp ~~ u 
			})
		}
		// return this value
		corps
}

