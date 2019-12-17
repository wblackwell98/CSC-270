import scala.io.Source
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import java.io._
import scala.annotation.tailrec
import java.util.Calendar

:load utilities.sc

val lib: CiteLibrary = loadLibrary("text/lesMiserables_fra.cex")

val tr: TextRepository = lib.textRepository.get

val corp: Corpus = tr.corpus

/* Let's make a *tokenized exemplar*! */

/*
What defines a word-break? Note the [ ], making this a regex…
… "any one of these"
*/
val splitters: String = """[“”“‘()‘\[\]·_…⸁.,:; "?·!⸂⸃–—]"""

/* We do this by mapping the .nodes of a Corpus
		1. For each .node in the Corpus…
		2. Split the .text into tokens
		2a. Optionally process the text in some way
		3. Attach an index-number to each token
		4. For each token, make a new URN that adds the index number
		5. With a URN and a Text (the token) you can make a CitableNode
		6. Return that Citable Node
*/

// We are defining a function here!
def tokenizeCorpus ( c: Corpus, exemplarLabel: String = "wt", corpFunc: Corpus => Corpus = c => c ): Corpus = {
	// for each node in our corpus…
	val tokenizedVector: Vector[CitableNode] = c.nodes.map( n => {

		// Grab the original URN
		val urnBase: CtsUrn = n.urn

		// Get its passage-component
		val citationBase: String = urnBase.passageComponent

		// Split up the text into tokens
		val tokenizedText: Vector[String] = splitWithSplitter(n.text, splitters)

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

	// The above returns a Vector[CitableNode], which we can turn into a Corpus…
	val tokCorpus: Corpus = Corpus(tokenizedVector)

	// Now we can apply final processing…
	corpFunc(tokCorpus)

}

// Functions you can pass to tokenizeCorpus…
def lowerCaseNodes( c: Corpus): Corpus = {
	val nodes: Vector[CitableNode] = {
		c.nodes.map( n => {
			CitableNode( n.urn, n.text.toLowerCase )
		})
	}
	Corpus(nodes)
}

def removeStopWords( c: Corpus ): Corpus = {
	val lcCorpus: Corpus = lowerCaseNodes(c)
	val stopWords: Vector[String] = "au aux avec ce ces dans de des du elle en et eux il je la le leur lui ma mais me même mes moi mon ne nos notre nous on ou par pas pour qu que qui sa se ses son sur ta te tes toi ton tu un une vos votre vous c d j l à m n s t y été étée étées étés étant suis es est sommes êtes sont serai seras sera serons serez seront serais serait serions seriez seraient étais était étions étiez étaient fus fut fûmes fûtes furent sois soit soyons soyez soient fusse fusses fût fussions fussiez fussent ayant eu eue eues eus ai as avons avez ont aurai auras aura aurons aurez auront aurais aurait aurions auriez auraient avais avait avions aviez avaient eut eûmes eûtes eurent aie aies ait ayons ayez aient eusse eusses eût eussions eussiez eussent ceci celà cet cette ici ils les leurs quel quels quelle quelles sans soi".split(" ").toVector
	val nodes: Vector[CitableNode] = {
		c.nodes.filter( n => {
			stopWords.contains(n.text) == false
		})
	}
	Corpus(nodes)
}

/* ------------------------

	 Below are two alternate ways to generate a tokenized corpus.
	 The first, the default, lower-cases all words first.
	 The second, commented-out by default, lowercases _and_ removes
	 English "stop-words".

   ------------------------  */

val tokenCorp: Corpus = tokenizeCorpus( corp, "lcTokens", lowerCaseNodes )
//val tokenCorp: Corpus = tokenizeCorpus( corp, "sigTokens", removeStopWords )

/* ------------------------ */


/* Citation-Aware N-Grams */

// Let's lose all the punctuation-tokens for this analysis…
val noPuncCorpus: Corpus = {
	// Remove tokens that consist only of punctuation
	val nodes: Vector[CitableNode] = {
		tokenCorp.nodes.filter( n => {
			n.text.replaceAll(punctuation,"").size > 0
		})
	}
	// replace punctuation within nodes
	val lcNodes: Vector[CitableNode] = nodes.map( n => {
		CitableNode( n.urn, n.text.toLowerCase.replaceAll(punctuation,"") )
	})
	// return a new, punctuation-free corpus, with stop words removed


	removeStopWords( Corpus( lcNodes ) )
}

// Using .sliding, let's get some N-grams… `n` is how many words in the pattern.

def makeNGramTuples( n: Int, c: Corpus ): Vector[(CtsUrn, String)] = {
	// Using .sliding, let's get all possible combinations of N tokens
	val slid: Vector[Vector[CitableNode]] = {
		c.nodes.sliding(n,1).toVector
	}
	/* Map this into a Vector[ (CtsUrn, String)]
		 return this value
	*/
	slid.map( s => {
		val newUrn: CtsUrn = s.head.urn.addPassage(
			s"${s.head.urn.passageComponent}-${s.last.urn.passageComponent}"
		)
		val newText: String = s.map( _.text ).mkString(" ")
		(newUrn, newText)
	})
}

/* Given a Vector of n-gram tuples, generated by makeNGramTuples, above, return
   a histogram. The value `filter` (defaults to 5) will eliminate any
   n-grams that don't occur at least 5 times.
*/

def makeNGramHisto( tups: Vector[(CtsUrn, String)], filter: Int = 5 ): Vector[(String, Int)] = {
	tups.map( _._2 ).view.groupBy( n => n).toVector.map( n => {
		( n._1, n._2.size )
	}).filter( _._2 >= filter).sortBy( _._2 ).reverse
}

/*
		Given an N-gram (e.g. "the people who"), return URNs identifying occurrances of it.
*/
def urnsForNGram( s:String, ngt: Vector[ (CtsUrn, String)] ): Set[CtsUrn] = {
	ngt.filter( _._2 == s ).map(_._1).toSet
}

/* Given a histogram, return the elements whose frequency sums to a given
   percentage of the whole.
*/
def takePercent( histo: Vector[(String, Int)], targetPercent: Int): Vector[(String, Int)] = {
	@tailrec def sumTakePercent(totalInstances: BigInt, h: Vector[(String, Int)], justNumbers: Vector[Int]): Vector[(String, Int)] = {
		val sum: BigInt = justNumbers.sum
		val currentPercent: Double = (sum.toDouble / totalInstances.toDouble) * 100
		if ( currentPercent <= targetPercent ) {
			h.sortBy(_._2).reverse
		} else {
			sumTakePercent( totalInstances, h.tail, justNumbers.tail )
		}
	}
	val t: BigInt = histo.map(_._2).sum
	val h: Vector[(String, Int)] = histo.sortBy(_._2) // we want _ascending_ order!
	val n: Vector[Int] = h.map(_._2) // we don't want to re-map the whole histo each time!
	sumTakePercent( t, h, n)
}


/* Do it! */

val ngt = makeNGramTuples(3, noPuncCorpus)
val ngh = makeNGramHisto(ngt)

def ngrams( n: Int, f: Int, c: Corpus = noPuncCorpus ): Vector[ ( String, Int) ] =
	{
		val ngt = makeNGramTuples(n, c)
		val ngh = makeNGramHisto(ngt, f)
		showMe( ngh.reverse )
		ngh

	}

showMe(ngh)

val jeanSpeaks: Set[CtsUrn] = urnsForNGram("dit jean valjean", ngt)
