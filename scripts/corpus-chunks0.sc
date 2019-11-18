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

/* Lets work with chunks of passages */

// Five passaages at a time?

val chunkSize: Int = 5

/* We want, instead of one Corpus, a Vector[Corpus] 
	 where each Corpus has 'chunkSize' passages, a number defined above
*/

val chunks: Vector[Corpus] = {
	corp.nodes.sliding(chunkSize, chunkSize).toVector.map( nv => Corpus(nv))
}

