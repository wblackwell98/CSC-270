:load utilities.sc

val lib: CiteLibrary = loadLibrary("text/lesMiserables_fra.cex")

val tr = lib.textRepository.get

val corp = tr.corpus

val justTextVec: Vector[String] = corp.nodes.map( _.text ) //This seperates the text from the urns

/*
val JustURNVec: Vector[CtsUrn] = corp.nodes.map( _.urn ) //This seperates the URNs from the text
*/
