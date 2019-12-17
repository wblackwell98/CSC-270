import scala.io.Source
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import java.io._
import scala.annotation.tailrec
import java.util.Calendar

:load utilities.sc

/* ***********************************************

EXERCISE IN READING ERROR MESSAGES!!!!!

This code WILL NOT RUN as-is.

Fix it so it will. There are notes and questions
scattered through the code, in comments.

Remember: Scroll up to the FIRST error. Fix that, then re-run.

*********************************************** */

val lib: CiteLibrary = loadLibrary("text/lesMiserables_fra.cex")

val tr: TextRepository = lib.textRepository.get

val cat: Catalog = tr.catalog

val corp: Corpus = tr.corpus



/* Let's turn a CTS text into a Vector[String] by word-tokenizing! */

/*
What defines a word-break? Note the [ ], making this a regex…
… "any one of these"
*/
val splitters: String = """[.,:; ?!]"""

/* We do this by mapping the .nodes of a Corpus
		1. For each .node in the Corpus…
		2. Get the .text part (a String)
		3. Split the .text into tokens
		4. “flatten” the result

*/


// for each node in our corpus…
val tokenizedVector: Vector[String] = corp.nodes.flatMap( n => {

	// for each node, get just the .text
	val txt: String = n.text.toLowercase // why is this an error?

	// Split up the text into tokens
	val tokenizedText: Vector[String] = txt.split(splitters).toVector // why won't this run?

	// We don't want empty nodes! So we filter them out…
	val noEmpties: Vector[String] = tokenizedText.filter( _.size > 0 )

	// we return this value…
	noEmpties

})

/*
		We can make a Histogram of words now…
*/

val tokenHisto: Vector[(String, Int)] = {

	// assemble words into groups…
	val grouped: Vector[( String, Vector[String])] = tokenizedVector.groupBy( t => t).toVector // Why an error here?

	// Map the _._2 part of tuple _out of_ a Vector[String] and _into_ its size…
	val remapped: Vector[ ( String, Int) ] = grouped.map( t => (t._1, t._2.size))

	// Sort…
	val sorted: Vector[ ( String, Int) ] =  remapped.sortBy( _._2 ).reverse

	// Return the histogram…
	sorted
}

showMe(tokenHisto)

/* Improvements…

	- Put the big numbers at the top?
	- Here "But" and "but" are two listings. How to fix this?	Where?

*/
