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

/* Let's make a *tokenized exemplar*! */

val exemplarLabel: String = "wt"

/* 
What defines a word-break? Note the [ ], making this a regex…
… "any one of these"
*/
val splitters: String = """[“”“‘()‘\[\]·_…⸁.,:; "?·!⸂⸃–—]"""

/* We do this by mapping the .nodes of a Corpus
		1. For each .node in the Corpus…
		2. Split the .text into tokens
		3. Attach an index-number to each token
		4. For each token, make a new URN that adds the index number
		5. With a URN and a Text (the token) you can make a CitableNode
		6. Return that Citable Node
*/


// for each node in our corpus…
val tokenizedVector: Vector[CitableNode] = corp.nodes.map( n => {

	// Grab the original URN
	val urnBase: CtsUrn = n.urn

	// Get its passage-component
	val citationBase: String = urnBase.passageComponent

	// Split up the text into tokens
	val tokenizedText: Vector[String] = n.text.split(splitters).toVector

	// We don't want empty nodes! So we filter them out…
	val noEmpties: Vector[String] = tokenizedText.filter( _.size > 0 )

	// Map these tokens into Citable Nodes
	val tokens: Vector[CitableNode] = noEmpties.zipWithIndex.map( z => {

		// By adding to the citation-component
		val citation: String = s"${citationBase}.${z._2 + 1}"

		// And creating a new URN
		val urn: CtsUrn = urnBase.addExemplar(exemplarLabel).addPassage(citation)

		// And getting the text of the new token-citable-node
		val passage: String = z._1

		// And making a CitableNode out of URN + Text
		CitableNode(urn, passage)
	})
	// we return this value…
	tokens
}).flatten

/* …About the .flatten at the end: We mapped a Vector[CitableNode] 
	 and each node became a Vector[CitableNode].
   So we have a Vector[Vector[CitableNode]], which we need to "flatten" into
   a Vector[CitableNode]. 

   If you have never tried to do that manually, in another language,
   you have no idea how grateful you are.
*/

// We make a Corpus out of our new Vector[CitableNode]
val tokenCorp: Corpus = Corpus(tokenizedVector)

/* Let's test it a little! */
val origSize: Int = corp.size // How many passage in the original?
val tokSize: Int = {
	tokenCorp.urns.map( _.collapsePassageBy(1) ).distinct.size
} // Do our tokens still represent the same number of passages?
assert( origSize == tokSize ) // They should!

/* These WILL NOT WORK for you with the default values!!!!! EDIT THEM!!!! */

val u1 = CtsUrn("urn:cts:greekLit:tlg0086.tlg035.ellis.wt:8.7.1.880")
val u2 = CtsUrn("urn:cts:greekLit:tlg0086.tlg035.ellis.wt:8.7.1")
val u3 = CtsUrn("urn:cts:greekLit:tlg0086.tlg035.ellis.wt:8.7")

/* Try some of these in the Console… */

showMe(tokenCorp ~~ u1)
showMe(tokenCorp ~~ u2)

val u2Text: String = (tokenCorp ~~ u2).nodes.map( _.text ).mkString(" ")
showMe( u2Text )

