import scala.io.Source
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import java.io._
import scala.annotation.tailrec

:load utilities.sc

println("\n\n\n\n")

val lib: CiteLibrary = loadLibrary("text/xxxx.cex")

val tr: TextRepository = lib.textRepository.get

val corp: Corpus = tr.corpus

