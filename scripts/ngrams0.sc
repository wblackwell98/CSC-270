import scala.io.Source
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import java.io._
import scala.annotation.tailrec
import java.util.Calendar

:load utilities.sc

val sampleText: String = """When Adam had lived one hundred thirty years, he became the father of a son in his likeness, according to his image, and named him Seth. The days of Adam after he became the father of Seth were eight hundred years; and he had other sons and daughters. Thus all the days that Adam lived were nine hundred thirty years; and he died. When Seth had lived one hundred five years, he became the father of Enosh. Seth lived after the birth of Enosh eight hundred seven years, and had other sons and daughters. Thus all the days of Seth were nine hundred twelve years; and he died. When Enosh had lived ninety years, he became the father of Kenan. Enosh lived after the birth of Kenan eight hundred fifteen years, and had other sons and daughters. Thus all the days of Enosh were nine hundred five years; and he died. When Kenan had lived seventy years, he became the father of Mahalalel. Kenan lived after the birth of Mahalalel eight hundred and forty years, and had other sons and daughters. Thus all the days of Kenan were nine hundred and ten years; and he died. When Mahalalel had lived sixty-five years, he became the father of Jared. Mahalalel lived after the birth of Jared eight hundred thirty years, and had other sons and daughters. Thus all the days of Mahalalel were eight hundred ninety-five years; and he died. When Jared had lived one hundred sixty-two years he became the father of Enoch. Jared lived after the birth of Enoch eight hundred years, and had other sons and daughters. Thus all the days of Jared were nine hundred sixty-two years; and he died. When Enoch had lived sixty-five years, he became the father of Methuselah. Enoch walked with God after the birth of Methuselah three hundred years, and had other sons and daughters. Thus all the days of Enoch were three hundred sixty-five years. Enoch walked with God; then he was no more, because God took him. When Methuselah had lived one hundred eighty-seven years, he became the father of Lamech. Methuselah lived after the birth of Lamech seven hundred eighty- two years, and had other sons and daughters. Thus all the days of Methuselah were nine hundred sixty-nine years; and he died.
"""

val splitters: String = """[.,:; ?!]"""

val cleanedUp: String = {
	sampleText.replaceAll(",","").replaceAll("  "," ").toLowerCase
}

val tokenized: Vector[String] = cleanedUp.split(splitters).toVector

val myN: Int = 3

val nGrams: Vector[ (String, Int) ] = {
	val slidVectors: Vector[ Vector[String] ] = {
		tokenized.sliding(myN, 1).toVector
	}
	val allNGrams: Vector[String] = {
		slidVectors.map( _.mkString(" "))
	}
	val groupedNGrams: Vector[ (String, Vector[String] )] = {
		allNGrams.groupBy( n => n ).toVector
	}

	val nGramHistos: Vector[ (String, Int) ] = {
		groupedNGrams.map( n => {
			(n._1, n._2.size)
		})
	}

	nGramHistos.sortBy(_._2).reverse
}

val filteredNGrams: Vector[ (String, Int) ] = {
		nGrams.filter( _._2 >= 2 )	
}

showMe(nGrams)

showMe(filteredNGrams)
