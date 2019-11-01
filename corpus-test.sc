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
val lib: CiteLibrary = loadLibrary("text/lesMiserables_fra.txt")

val tr: TextRepository = lib.textRepository.get

val corp: Corpus = tr.corpus

// Test things…
val nodes: Vector[CitableNode] = corp.nodes

val howManyNodes: Int = nodes.length

val corpusUrns: Vector[CtsUrn] = corp.urns

val firstNode: CitableNode = corp.first

val firstNodeUrn: CtsUrn = corpusUrns.head

val firstNodeText: String = firstNode.toString
