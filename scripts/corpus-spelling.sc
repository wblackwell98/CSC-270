
:load scripts/corpus-tokenize.sc


val spellCheckSplitters: String =  """[()\[\]·⸁.,; "?·!–—⸂⸃-]"""

val lib: CiteLibrary = loadLibrary("text/arist_politics.cex")

val standardDict: Vector[String] = loadFile("validation-data/SCOWL-wl/words.txt")
val userDict: Vector[String] = loadFile("validation-data/userDictionary.txt")
val words: Vector[String] = standardDict ++ userDict

val tr: TextRepository = lib.textRepository.get

val cat: Catalog = tr.catalog

val corp: Corpus = tr.corpus

/* Let's make a *tokenized exemplar*! */

def processForSpelling( c: Corpus ) = {
	// We're keeping the ' char and the hyphen!
	//    (That's why this is different from the 'punctution' value in utilities.sc)
	val removePunctuation: String = """[“”“‘()‘’\[\]_·…⸁.,:; "?·!⸂⸃—]"""

	val filterNodes: Vector[CitableNode] = c.nodes.filter( n => {
			removePunctuation.contains(n.text) == false
	})

	val mappedNodes: Vector[Option[CitableNode]] = c.nodes.map( n => {
		val replacedText: String = n.text.replaceAll(removePunctuation,"")
		replacedText.size match {
			case i if (i == 0) => None
			case _ => Some( CitableNode( n.urn, replacedText ) )
		}
	})

	val finalNodes: Vector[CitableNode] = {
		mappedNodes.filter( m => {
			m != None
		}).map( m => m.get)
	}

	Corpus(finalNodes)

}

val spellCheckCorpus: Corpus = tokenizeCorpus( corp, "spellTokens", processForSpelling )


/* We don't want to spell-check the same word again and again.
	 But we _do_ want to know the citations for each instance of a word.
*/

val wordIndex: Vector[ (String, Vector[CtsUrn]) ] = {
	spellCheckCorpus.nodes.groupBy( n => n.text ).toVector.map( m => {
		val word: String = m._1
		val urnIndex: Vector[CtsUrn] = m._2.map(_.urn)
		(word, urnIndex )
	}).sortBy( m => m._2.size).reverse
}

/*  Spell-Check!
		Here's a problem… capitalized normal words.
		The word-list has "the" but not "The".
		We don't want to lower-case all words, because it has "Paris" but
		not "paris".			
		So we check for both…
*/
val badWords1: Vector[ (String, Vector[CtsUrn]) ] = {
	wordIndex.filter( wi => {

		val hasRegular: Boolean = words.contains(wi._1) 
		val hasLowerCase: Boolean = words.contains(wi._1.toLowerCase) 
		// If either (`or`) is true, say "true"
		val inDictionary: Boolean = hasRegular | hasLowerCase 
		// But remember… we want _bad_ words!
		inDictionary == false
	})
}

println("\n----------\nBad Words (before testing hyphenated words)\n-----------\n")
showMe( badWords1.map(_._1))

/* Observed problems!
	 - we need to filter out tokens that are all just arabic numbers.
	 - we need to split hyphenated words, and check both sides.
	 - perhaps we need to find a list of geographic names?
*/

val badWords2: Vector[ (String, Vector[CtsUrn]) ] = {

	wordIndex.filter( wi => {
		val w: String = wi._1 // grab the word

		val hasRegular: Boolean = words.contains(w) 
		val hasLowerCase: Boolean = words.contains(w.toLowerCase) 
		val hyphenatedCheck: Boolean = {
			if (w.contains("-")) {
				val w1: String = w.split("-").toVector.head
				val w2: String = w.split("-").toVector.tail.mkString
				if (words.contains(w1) & words.contains(w2)) {
					true
				} else false
			} else false
		}
		// If either (`or`) is true, say "true"
		val inDictionary: Boolean = hasRegular | hasLowerCase | hyphenatedCheck
		// But remember… we want _bad_ words!
		inDictionary == false
	})
}

println("\n----------\nBad Words (after testing hyphenated words)\n-----------\n")
showMe( badWords2.map(_._1))
