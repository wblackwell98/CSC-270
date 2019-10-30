import scala.io.Source
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import java.io._
import scala.annotation.tailrec

:load utilities.sc

// Edit the following to load *your* text!
val lib: CiteLibrary = loadLibrary("text/arist_politics.cex")

val tr: TextRepository = lib.textRepository.get

val cat: Catalog = tr.catalog

val corp: Corpus = tr.corpus


/* Make a Character-Histogram

	 For this (but not for the next part) we don't need a Corpus…
	 …just a big Vector of Characters.
*/

val charHisto: Vector[(Char, Int)] = {

	// For each node in the Corpus, keep only the text-part (toss the URN)
	val justText: Vector[String] = corp.nodes.map( _.text )

	// Map each element of that Vector to a Vector[Char]
	//      (Remember that a String is, really, just a Vector[Char] anyway!)
	val makeChars: Vector[ Vector[Char] ] = justText.map( _.toVector )

	// Nested Vectors are confusing… let's flatten it
	val flatCharVec: Vector[ Char ] = makeChars.flatten

	// You've done this before
	val grouped: Vector[ ( Char, Vector[Char] ) ] = flatCharVec.groupBy( c => c ).toVector

	// return the result of the following as the value for charHisto
	//		We re-map the 'grouped' so instead of _._2 being a Vector, we just get the
	//		_size_ of the vector.
	//		Then we sorty by that number, and reverse it to get the big numbers at the top.
	grouped.map( g => {
		( g._1, g._2.size )
	}).sortBy( _._2 ).reverse

}

// Type 'showMe(charHisto)' to see the result
// Some other things you can play with:
val charList: Vector[Char] = charHisto.map( _._1 ).sortBy( c => c ) // just a list of chars
val charListString: String = charList.map( c => s"'${c}'").mkString(" ") // the above turned into a String

/* ---------------------------------------- */
/* Character validation */

// Make a vector of legit characters. Make it the easy way!
val goodChars: Vector[Char] = """ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890—abcdefghijklmnopqurstuvwxyz.,? """.toVector


// Find the bad characters by doing .diff on two Vectors
//		Think of it as subtraction:
//		charList - goodChars = [only bad chars]
val badCharsInText: Vector[Char] = charList.diff(goodChars)
// if badCharsInText is an empty Vector, you have validated your text's character-set!




/* 
		We can make one Corpus out of another… we could make a Corpus of only the
		invalid characters…
*/
val badCharCorpus: Corpus = {
	// Filter the contents of 'corp' by omitting any nodes that have _no_ bad chars
	val badNodes: Vector[CitableNode] = corp.nodes.filter( n => {
		val chars: Vector[Char] = n.text.toVector.distinct
		// vectorA.diff(vectorB) is something you can play with in the console…
		chars.diff(goodChars).size > 0
	})
	// For each of the offending nodes, make a new version that has only the bad chars
	val boiledDown: Vector[CitableNode] = badNodes.map( n => {
		val u: CtsUrn = n.urn
		val cc: Vector[Char] = n.text.toVector.distinct
		val badCharString: String = cc.diff(goodChars).mkString(" ")
		// return the following as the value of boiledDown
		CitableNode(u, badCharString)
	})
	// make a Corpus out of the Vector[CitableNode] you got in boiledDown
	Corpus(boiledDown)
}

/* 

		- Type 'badCharsInText' or 'showMe(badCharCorpus)' to see the result.
		- Look at what is getting flagged as a "bad" character.
		- If you see something wrongly flagged, add it to `goodChars` above.
		- If something needs changing, think about it and consider editing the .cex file.
		- Re-run until you are seeing only legitimately bad characters.
		- Add to this script a function or two to generate a Markdown table of the official
		  good characters, with their Unicode codepoints…

*/


/*	 
		 Given a Char, return a String that gives its info.
		 Bonus: When reporting characters, replace invisibles with their names…
		 ( This is the first time you've seen the 'match' construction )
*/
def reportChar( c: Char ): String = {
	c match {
		case ' ' => s"`space` (${c.toHexString})" 
		case '\r' => s"`return` (${c.toHexString})" 
		case '\t' => s"`tab` (${c.toHexString})" 
		case _ => s"`${c}` (${c.toHexString})"
	}
}


/* 
		A custom Function that takes a String of characters and a 'width',
		and returns a Markdown-formatted table, 'width'-columns wide.
		Run this script and look at the file charTable.md to see the output.
*/
def charTable( s:String, width: Int = 5 ): String = {
		// Make the table header…
		val headerString: String = {
			 val lineOne: String = ( 1 to width ).toVector.map( i => { s"| Character "}).mkString
			 val lineTwo: String = (1 to width).toVector.map( i => {    "|-----------" }).mkString
			 s"${lineOne}|\n${lineTwo}|\n"
		}
		val cVec: Vector[Vector[Char]] = s.toVector.sortBy( c => c).sliding( width, width).toVector
		val tableString: String = cVec.map( v => {
			v.map( c => {
				val s: String = reportChar(c)
				s"| ${s} "
			}).mkString + "|"
		}).mkString("\n")
		headerString + tableString
}

saveString( charTable(charList.mkString), "", "charTable.md")