package edu.furman.classics.csc270

import better.files._
import java.io.{File => JFile}
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import scala.annotation.tailrec


object demo {

	def addOne( i: Int ): Int = {
		i + 1
	}

	def addOne ( s: String ): String = {
		s"${s} + 1"	
	}

}
