import scala.io.Source
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import java.io._
import scala.annotation.tailrec

:load utilities.sc

val workStr: String = "urn:cts:test:group.work:"

val workU: CtsUrn = CtsUrn(workStr)
val versionU: CtsUrn = workU.addVersion("version")


val psg0: CtsUrn = versionU.addPassage("3")
val psg1: CtsUrn = versionU.addPassage("3.1")
val psg2: CtsUrn = versionU.addPassage("3.2")
val psg3: CtsUrn = psg2.dropVersion

val longPsg1: CtsUrn = versionU.addPassage("3.def.3.title.2")

longPsg1.collapsePassageTo(1)
longPsg1.collapsePassageTo(2)

psg0 > psg1
psg0 < psg1
psg0 ~~ psg1
psg2 ~~ psg3
psg2 > psg3
psg3 > psg2

psg0.citationDepth
psg1.citationDepth
longPsg1.citationDepth

// Ranges

val range1: CtsUrn = CtsUrn("urn:cts:test:group.work:1-2")
val range2: CtsUrn = range1.addPassage("1.1-1.40")

range1.rangeBegin
range1.rangeEnd
range2.rangeBegin
range2.rangeEnd


