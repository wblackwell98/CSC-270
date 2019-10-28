import scala.io.Source
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import java.io._
import scala.annotation.tailrec

:load utilities.sc

println("\n\n\n\n")

/* Test: 
		Edit this script, replacing 'xxxx' with something else,
		so the script runs and gives good data.

		Submit your work by committing a new version of this script
		to GitHub! (That is part of the assignment.)
*/


// Test thing…
val lib: CiteLibrary = loadLibrary("text/xxxx.cex")

val tr: TextRepository = lib.textRepository.get

val corp: Corpus = tr.corpus

// Test things…
val nodes: Vector[xxxx] = corp.nodes

val howManyNodes: Int = xxxx

val corpusUrns: Vector[CtsUrn] = xxxx

val firstNode: CitableNode = xxxx

val firstNodeUrn: CtsUrn = xxxx

val firstNodeText: String = xxxxx

