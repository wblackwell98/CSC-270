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

/* Lets work chapter-by-chapter */

// There are a couple of ways of doing this; which is faster?








// Way 1: Using the OHCO2 Library naively…
val chapterCorporaOne: Vector[Corpus] = {
	// Get a Vector of chapter-level URNs…
	val chapterUrns: Vector[CtsUrn] = {
		corp.urns.map( _.collapsePassageTo(1) ).distinct
	}
	// "twiddle" the big corpus into little corpora…
	chapterUrns.map( cu => {
	 		corp ~~ cu
	})
}










// Way 2, being more thoughtful about Corpus-Algebra
val chapterCorporaTwo: Vector[Corpus] = {
	// Get a Vector of chapter-level URNs…
	val chapterUrns: Vector[CtsUrn] = {
		corp.urns.map( _.collapsePassageTo(1) ).distinct
	}
	// Be more specific than a generic "twiddle"
	chapterUrns.map( cu => {
	 		Corpus( corp.nodes.filter( _.urn.collapsePassageTo(1) == cu ))	
	})
}








/* 
			Way 3, using a function from the OHCO2 library…
			… that consists of 52 lines of code (complex, had to be
			tested and de-bugged), but which takes advantage of Scala's
			list-processing efficiency to do the job we need doing.
*/

val chapterCorporaThree: Vector[Corpus] = {
	corp.chunkByCitation(1)
}


