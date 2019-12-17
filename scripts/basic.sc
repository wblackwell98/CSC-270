
// n.g. This depends on some Imported code libraries that are imported in Utilities.sc

:load utilities.sc


val lib: CiteLibrary = loadLibrary("text/lesMiserables_fra.cex")

val tr: TextRepository = lib.textRepository.get

val corp: Corpus = tr.corpus

// How to turn a Corpus into One Big String
val oneBigString: String = corp.nodes.map( _.text ).mkString(" ")

// How to split a Corpus into bits
//       Note: `punctuation` is defined in utilities.sc

val tokens: Vector[String] = oneBigString.split(punctuation).toVector

// How to get rid of empty tokens

val noEmpties: Vector[String] = tokens.filter( _.size > 0 )
