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

	// Now e can apply final processing…
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
	val stopWords: Vector[String] = Vector("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "you're", "you've", "you'll", "you'd", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "she's", "her", "hers", "herself", "it", "it's", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "that'll", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very", "s", "t", "can", "will", "just", "don", "don't", "should", "should've", "now", "d", "ll", "m", "o", "re", "ve", "y", "ain", "aren", "aren't", "couldn", "couldn't", "didn", "didn't", "doesn", "doesn't", "hadn", "hadn't", "hasn", "hasn't", "haven", "haven't", "isn", "isn't", "ma", "mightn", "mightn't", "mustn", "mustn't", "needn", "needn't", "shan", "shan't", "shouldn", "shouldn't", "wasn", "wasn't", "weren", "weren't", "won", "won't", "wouldn", "wouldn't")
	val nodes: Vector[CitableNode] = {
		c.nodes.filter( n => {
			stopWords.contains(n.text) == false
		})
	}
	Corpus(nodes)
}

// Let's make some tokenized corpora!
//val tokenCorpus: Corpus = tokenizeCorpus( corp, "tokens" )
//val lcTokenCorpus: Corpus = tokenizeCorpus( corp, "lcTokens", lowerCaseNodes )
//val noStopWordsCorpus: Corpus = tokenizeCorpus( corp, "sigTokens", removeStopWords )

