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

/* 

		This is… a little complicated… harder than chunking by number of citable nodes!

		Chunk a Corpus into a Vector[Corpus] in which each member has approximately
		the same number of characters. Not so useful for the Iliad, but quite useful
		for a novel, in which paragraphs of dialogue may be very short, but paragraphs
		of description, very long.

		This is a "tail-recursive" function.
*/

def equalSize( origCorpus: Corpus, target: Int): Vector[Corpus] = {
	// This is the tail-recursive bit…
	// 'resultCorpusVec' is the "accumulator"
	// 'whatsLeft' is the unprocessed part of the original Corpus
	// 'target' is the number of chars we want to aim for
	@tailrec def recurseEqualSize( resultCorpusVec: Vector[Corpus], whatsLeft: Corpus, target: Int): Vector[Corpus] = {

		// First, we see the size of the latest Corpus in the list
		//		We start with an empty accumulator, so we need to check for that possibility
		val workingCorpusSize: Int = {
			if (resultCorpusVec.size == 0) 0
			else {
				// Take the last Corpus in the list; count its characters.
				resultCorpusVec.last.nodes.map(_.text).mkString.size
			}
		}

		/* Three possibilities…
	 		 Case 1. There is only one CitableNode left in whatsLeft
	 		 Case 2. We've just met the target
	 		 Case 3. We haven't met the target
		*/
		if ( whatsLeft.size == 1) { 
			// Case 1: Add it and recurse
			val newResultVec: Vector[Corpus] = resultCorpusVec :+ whatsLeft
			newResultVec
		} else if (workingCorpusSize >= target) { 
			// Case 2: Recurse with an empty final Corpus as the '.lates' in results
			val emptyNewCorpus: Corpus = Corpus(Vector[CitableNode]())
			val newResultVec: Vector[Corpus] = resultCorpusVec :+ emptyNewCorpus
			recurseEqualSize( newResultVec, whatsLeft, target)
		} else {
			// Case 3: Add one more node to the latest Corpus, recurse
			val workingCorpus: Corpus = {
				// The very first time through, we'll have an empty Corpus, so check for this
				if ( resultCorpusVec.size == 0 ) {
					Corpus(Vector[CitableNode]())
				} else {
					resultCorpusVec.last	
				}
			}
			// All the untreated citable nodes…
			val poolNodes: Vector[CitableNode] = whatsLeft.nodes
			// Add the next node to our working corpus
			val expandedCorp: Corpus = workingCorpus ++ Corpus(Vector(poolNodes.head))
			// Remove that node from whatsLeft
			val newWhatsLeft: Corpus = Corpus(poolNodes.tail)
			// Add the new version of the working corpus to results
			val newResultCorpusVec: Vector[Corpus] = resultCorpusVec.dropRight(1) :+ expandedCorp
			// Recurse!
			recurseEqualSize( newResultCorpusVec, newWhatsLeft, target)
		}
	}

	// Invoke the recursive function for the first time.
	val answer: Vector[Corpus] = recurseEqualSize( Vector[Corpus](), origCorpus, target)
	answer
}


// A little method to spit out stats on a Vector[Corpus]…
// … each starting URN, how many passage, and the total char-count
def proveIt( v: Vector[Corpus]): Unit = {
	for (c <- v) {
		val ccs: Int = c.nodes.map( _.text ).mkString.size
		val startU: CtsUrn = c.urns.head
		val numUs: Int = c.urns.size
		println(s"${startU}\t\t${numUs} nodes\t\t${ccs} chars")
	}
}

// Using the above, do it, and see how long it takes, just for fun
val charChunks: Vector[Corpus] = {
	val timeStart1 = Calendar.getInstance().getTimeInMillis()
	val answer = equalSize( corp, 5000 )
	val timeEnd1 = Calendar.getInstance().getTimeInMillis()
	val elapsedTime: Double = (timeEnd1 - timeStart1) / 1000
	println( s"Finished in ${elapsedTime} seconds." )
	answer
}