package edu.furman.classics.csc270

import better.files._
import java.io.{File => JFile}
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import scala.annotation.tailrec


object hocuspocus {

	/** Split a Corpus in to a Vector[Corpus] by citation
  * (Will first chunk by Text). 
  * @param drop How many levels of the passage-hierarchy, from the right, to drop when grouping
  */
  def chunkByCitation(corp: Corpus, drop: Int = 1): Vector[Corpus] = {
    val textChunks: Vector[Corpus] = corp.chunkByText
    val sectionChunks: Vector[Corpus] = textChunks.map( tc => {
      val deepestLevel: Int = tc.urns.map(_.citationDepth.head).min
      //println(s"deepestLevel = ${deepestLevel}")
      if (deepestLevel <= drop) {
      	println(s"\n-------\nWARNING: drop = ${drop}. This is larger than, or equal to, ${deepestLevel}, the minimum citation depth in text ${tc.nodes.head.urn.dropPassage}. The corpus will not be divided up!\n------\n")
        val vc: Vector[Corpus] = Vector(tc)
        vc
      }
      else {
         //println(s"drops = ${drops}")
         val urnMap: Vector[(CtsUrn, Int)] = tc.urns.zipWithIndex
         // Get vector of node-indices where breaks will happen
         val breakPoints: Vector[Int] = {
         	val groupedVec: Vector[ (CtsUrn, Vector[(CtsUrn, Int)])] = {
         		urnMap.groupBy( um => um._1.collapsePassageBy(drop)).toVector
         	}
         	val justHeads: Vector[ Int ] = {
         		groupedVec.map( _._2.head._2).sortBy( i => i )
         	}
         	justHeads
         }
         // Turn that into a map of from-Index, to-Index
         val chunkMap: Vector[Vector[Int]] = {
            val slid: Vector[Vector[Int]] = breakPoints.sliding(2,1).toVector
            // make a pair for the last chunk
            val lastPair: Vector[Vector[Int]] = {
              val v: Vector[Int] = Vector( slid.last.last, (urnMap.last._2 + 1) )
              Vector(v)
            }
            val adjustedSlid: Vector[Vector[Int]] = slid.map( s => {
              Vector(s(0), s(1))
            }) ++ lastPair
            //println(s"${adjustedSlid}")
            adjustedSlid
         }
         val corpVec: Vector[Corpus] = chunkMap.map( cm => {
            val fromIndex:Int = cm(0)
            val untilIndex:Int = cm(1)
            val nodeVec: Vector[CitableNode] = {
              tc.nodes.slice(fromIndex, untilIndex)
            }
            val newCorpus: Corpus = Corpus(nodeVec)
            newCorpus
         })
         corpVec
      }
    }).flatten
    sectionChunks
  }

	/* 
			Divides a Corpus by dropping `drop` levels of the passage-hieararchy
	*/
	def corpusToChapters( corp: Corpus, drop: Int = 1 ): Vector[Corpus] = {
		chunkByCitation(corp, drop)
	}

	/* 
			Divides a Corpus into Corpora containing equal numbers
			of passages (regardless of the length of the passage). 
			`n` is how many Corpora you want.		

			If there is a remainder, you will get n+1 corpora.
	*/
	def equalDivs( corp: Corpus, n: Int = 10 ): Vector[Corpus] = {
			val total: Int = corp.urns.size
			val eachSize: Int = total / n
			val corpVec: Vector[Corpus] = corp.nodes.sliding( eachSize, eachSize ).toVector.map( nc => {
				Corpus(nc)
			})
			corpVec
	}

	def equalSize( corp: Corpus, target: Int = 5000 ): Vector[Corpus] = {
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
		val answer: Vector[Corpus] = recurseEqualSize( Vector[Corpus](), corp, target)
		answer
	}

	def html( corp: Corpus): String = {
		val nodeString: String = corp.nodes.map( n => {
				val passage: String = n.urn.passageComponent
				s"""<div class="cts_node"><span class="cts_ref">${passage}</span><span class="cts_passage">${n.text}</span></div>"""
		}).mkString("\n")
		s"""<div class="cts_corpus">${nodeString}</div>"""
	}


}

object catacomb {

	def catString( cat: CatalogEntry ): String = {
		cat.toString
	}

	def catMarkdown( cat: CatalogEntry ): String = {
		val gn: String = cat.groupName
		val wt: String = cat.workTitle
		val cs: String = cat.citationScheme
		val lang: String = cat.lang
		val vl: String = cat.versionLabel.getOrElse("")
		val el: String = cat.exemplarLabel.getOrElse("")
		
		s"""${gn}, *${wt}*. ${vl} [${lang}]. Cited by ${cs}."""

	}

	def html( cat: CatalogEntry ): String = {
		val gn: String = cat.groupName
		val wt: String = cat.workTitle
		val vl: String = cat.versionLabel.getOrElse("")
		val el: String = cat.exemplarLabel.getOrElse("")
		val cs: String = cat.citationScheme
		val lang: String = cat.lang
		s"""
		<div class="cts_catalog">
		<p>${gn}, <span class="cts_workTitle">${wt}</span>. ${vl} [${lang}]. Cited by <span class="cts_citationScheme">${cs}</span>.</p>
		</div>
		"""
	}

}
