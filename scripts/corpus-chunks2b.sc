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

// Way 1, mostly pure Scala
val timeStart1 = Calendar.getInstance().getTimeInMillis()
val chapterCorporaOne: Vector[Corpus] = {
	val chapterUrns: Vector[CtsUrn] = {
		corp.urns.map( _.collapsePassageTo(1) ).distinct
	}
	chapterUrns.map( cu => {
	 		corp ~~ cu
	})
}
val timeEnd1 = Calendar.getInstance().getTimeInMillis()
println( s"chapterCorporaOne in ${timeEnd1 - timeStart1} milliseconds." )

// Way 2, mostly pure Scala, being more thoughtful about Corpus-Algebra
val timeStart2 = Calendar.getInstance().getTimeInMillis()
val chapterCorporaTwo: Vector[Corpus] = {
	val chapterUrns: Vector[CtsUrn] = {
		corp.urns.map( _.collapsePassageTo(1) ).distinct
	}
	chapterUrns.map( cu => {
	 		Corpus( corp.nodes.filter( _.urn.collapsePassageTo(1) == cu ))	
	})
}
val timeEnd2 = Calendar.getInstance().getTimeInMillis()

// Way 3, using stuff from the OHCO2 library
val timeStart3 = Calendar.getInstance().getTimeInMillis()
val chapterCorporaThree: Vector[Corpus] = {
	corp.chunkByCitation(1)
}
val timeEnd3 = Calendar.getInstance().getTimeInMillis()

println( s"\n\nchapterCorporaOne in ${timeEnd1 - timeStart1} milliseconds." )
println( s"chapterCorporaTwo in ${timeEnd2 - timeStart2} milliseconds." )
println( s"chapterCorporaThree in ${timeEnd3 - timeStart3} milliseconds." )

println( s"\n( chapterCorporaOne == chapterCorporaTwo ) = ${ chapterCorporaOne == chapterCorporaTwo }.")
println( s"( chapterCorporaOne == chapterCorporaThree ) = ${ chapterCorporaOne == chapterCorporaThree }.")

// type, e.g. 'showMe(chapterCorporaTwo(0))' to list results

