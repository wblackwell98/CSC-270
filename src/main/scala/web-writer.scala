package edu.furman.classics.csc270

import better.files._
import java.io.{File => JFile}
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import scala.annotation.tailrec


object webWriter {

	val htmlDirectory: String = "html/pages/"

  /** Our main function where the action happens */
  def main(args: Array[String]) {
  		// Find out of the user typed the right command to launch this!

  		// Get the parameters that came with 'run'
  		val params: Vector[String] = args.toVector
  		val libraryPath: Option[String] = {
  			if ( params.size > 0 ) Some(params.head)
  			else {
  				println(s"""\n------\nRun by entering the following into SBT Console: `run path/to/lib.cex`.\n------\n""")
  				None
  			}
  		}
  		libraryPath match {
  			case Some(lp) => {
  				 if (lp.endsWith(".cex")) makePages( lp )
  				 else {
  				 	println(s"""\n------\nParameter `${lp}` does not seem to be a Unix path to a .cex library file.\n------\n""")
  				 }
  			}
	  		case None => // do nothing
  		}
  		
  }


  /*
			the `main` function kicks it off, but the real work happens here…
  */
  def makePages( cexFilePath: String ): Unit = {

  	try {

		  	println(s"\n--------\nconstructing website\n--------\n")
				/* Steps */
				// 1. Get our Data…
				val lib: CiteLibrary = utilities.loadLibrary(cexFilePath)
				val tr: TextRepository = lib.textRepository.get
				val cat: Catalog = tr.catalog
				val corp: Corpus = tr.corpus
				println(" \n ")
				println(s"Loaded…\n${cat.toString}")
				println(s"Corpus size = ${corp.size}")
				println(" \n ")

				// 2. Split it into manageable chunks. THERE ARE SEVERAL WAYS TO DO THIS!
				//val corpVec: Vector[Corpus] = hocuspocus.corpusToChapters( corp, drop = 2 )
				//val corpVec: Vector[Corpus] = hocuspocus.equalDivs( corp, n = 5 )
				val corpVec: Vector[Corpus] = hocuspocus.equalSize( corp, target = 5000 )
				println(" \n ")
				println(s"Will write ${corpVec.size} pages…")
				println(" \n ")

				// 3. For each chunk, make an HtmlCorpus object (see below)
				val htmlVec: Vector[HtmlCorpus] = vecToHtmlCorpora(cat, corpVec)

				// 4. For each of those, write it to a file.
				val htmlDir: File = File(htmlDirectory)
				htmlDir.clear()
				for ( hv <- htmlVec) {
					hv.save( filePath = htmlDirectory)
				}

				// 5. Write the landing page, with the toc.	
				landingPage(htmlVec)

		} catch {
			case e: Exception => {
				println(s"""\n-----\nBuilding the site failed with the following error: \n\n${e}\n\n-----\n)""")
			}
		}
  }


  /* 
				A function: Given a URN and a number, generate a file-name.
  */
  def urnToFileName( urn: CtsUrn, index: Option[Int] = None ): String = {
  	val baseName: String = urn.workParts.mkString("_")
  	index match {
  		case Some(i) => {
  			s"${baseName}_${i}.html"
  		}
	  	case None => {
	  		s"${baseName}.html"
	  	}
  	}
  }

  /* 
			We define a new object or "class", HtmlCorpus, that has all the 
			data we need to write HTML pages for each chunk.

			This class has a .toString method, and a .html method that does 
			our work for us.
  */
	case class HtmlCorpus(
		corp: Corpus,
		cat: CatalogEntry,
		fileName: String,
		index: Int,
		howMany: Int,
		prevFileName: Option[String],
		nextFileName: Option[String]
	) {

		override def toString: String = {
			"HtmlCorpus"
		}

		def save(filePath:String = "html/"):Unit = {
			val file = File(filePath + fileName)
			file.overwrite(this.html)
		}


		def html: String = {
			val catString: String = catacomb.html(cat)	
			val corpString: String = hocuspocus.html(corp)

			val titleString: String = {
				val fromPsg: String = corp.nodes.head.urn.passageComponent
				val toPsg: String = corp.nodes.last.urn.passageComponent
				s"""<div class="cts_corpusTitle">${fromPsg}-${toPsg}</div>"""
			}

			val sequenceString: String = {
				s"""<div class="cts_progress"><progress class="cts_progress" max="${howMany}" value="${index + 1}"/></div>"""
			}

			val navString: String = {
				val prevStr: String = prevFileName match {
					case Some(fn) => {
						s"""<span class="cts_prev"><a href="${fn}"> ⇽ </a></span>"""
					}
					case None => ""
				}

				val nextStr: String = nextFileName match {
					case Some(fn) => {
						s"""<span class="cts_next"><a href="${fn}"> ⇾ </a></span>"""
					}
					case None => ""
				}

				val middleStr: String = {
					if ( (prevFileName != None) && (nextFileName != None) ) " | " else ""
				}

				s"""<div class="cts_nav">${prevStr}${middleStr}${nextStr}</div>"""

			}

			// Here's the result…
			s"""
			<!doctype html>
			<html>
			<head>
			<title>${fileName}</title>
			<link rel="stylesheet" href="../style.css">
			</head>
			<body>
			<header>Your header</header>
			<article>
			${sequenceString}
			${catString}
			<div class="cts_tocLink"><a href="index.html">Table of Contents</a></div>
			${navString}
			${titleString}
			${corpString}
			${navString}
			</article>
			<footer>Your footer</footer>
			</body>
			</html>
			"""

		}

	}

	/*
			This is the function that takes a Vector[Corpus] and turns it into
			a Vector[HtmlCorpus]
	*/
	def vecToHtmlCorpora( cat: Catalog, vcorp: Vector[Corpus]): Vector[HtmlCorpus] = {
		vcorp.zipWithIndex.map( ic => {
			val corp: Corpus = ic._1
			val urn: CtsUrn = corp.urns.head.dropPassage
			val catEntry: CatalogEntry = {
				val u = corp.urns.head.dropPassage
				cat.entriesForUrn(u).head
			}
			val index: Int = ic._2
			val fileName: String = { urnToFileName(urn, Some(index)) }
			val howMany: Int = vcorp.size
			val prevFileName: Option[String] = {
				if (index == 0) None
				else Some(urnToFileName(urn, Some(index - 1)))
			}
			val nextFileName: Option[String] = {
				if ((index + 1) == howMany) None
				else Some(urnToFileName(urn, Some(index + 1)))
			}
			HtmlCorpus( corp, catEntry, fileName, index, howMany, prevFileName, nextFileName)
		})
	}

	/*
			This function creates a "landing page" for a Vector[HtmlCorpus]
			containing a linked table-of-contents. For fancy!
	*/
	def landingPage( vcorp: Vector[HtmlCorpus]): Unit = {

		val workCat: CatalogEntry = vcorp.head.cat

		val catString: String = catacomb.html(workCat)	

		val titleString: String = workCat.toString // can fancify this
	
		val infoString: String = """<div class="cts_siteDesc">Whatever you want to say here.</div>"""		

		val tocHeader: String = """<div class="cts_toc">Table of Contents</div>"""

		val tocEntries: Vector[String] = vcorp.map( vc => {
			s"""<li class="cts_tocEntry"><span class="cts_tocIndex">${vc.index + 1}.</span> 
			<a href="${urnToFileName(vc.corp.nodes.head.urn.dropPassage, Some(vc.index))}">
			<span class="cts_tocBit">${vc.corp.nodes.head.urn.passageComponent}</span><span class="cts_tocHyphen"/><span class="cts_tocBit">${vc.corp.nodes.last.urn.passageComponent}</span>
			</a></li>"""
		})

		val toc: String = {
			"""<ul class="cts_toc">""" + tocEntries.mkString("\n") + "</ul>"
		}

		val htmlString = s"""
			<!doctype html>
			<html>
			<head>
			<meta charset="utf-8"/>
			<title>${titleString}</title>
			<link rel="stylesheet" href="../style.css">
			</head>
			<body>
			<header>Your header</header>
			<article>
			${catString}
			${infoString}
			${tocHeader}
			${toc}	
			</article>
			<footer>Your footer</footer>
			</body>
			</html>
		"""

		val file: File = File(htmlDirectory + "index.html")
		file.overwrite( htmlString )

	}



}
