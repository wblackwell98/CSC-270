import scala.annotation.tailrec

// A non-functional (but working!) example using a 'var' and a 'loop'
def nonFunctionalSum( n: Int): Long = {
	var answer = 0
	for ( i <- (1 to n)) answer = answer + i
	answer
}





def badSum( n: Long) : Long = {
	def getBadSum(n: Long): Long = {
		if (n == 0) n // return 'n'
		else n + badSum(n - 1) // call the outer function. Recursion!
	}
	val sumOfMyNumber: Long = getBadSum(n)
	sumOfMyNumber
}





def goodSum(n: Long): Long = {
	@tailrec 
	def sumTailRec(runningTotal: Long, n: Long): Long = {
		if (n == 0) runningTotal 
		else sumTailRec(n + runningTotal, n - 1)
	}
	sumTailRec(0, n)
}





/* Scala has this stuff built-in, with the "reduceLeft" function… */

// Factorial
def bestFact(i: Int): BigInt = {
	// The first part is geting the numbers
	val vi: Vector[Int] = (1 to i).toVector
	// Now map each one to a BigInt, 'cause we'll need it!
	val vbi: Vector[BigInt] = vi.map( BigInt(_) )
	// Now we can do a factorial…
	vbi.reduceLeft( _ * _ )
}	

// Sum
def bestSum(i: Int): BigInt = {
	// The first part is geting the numbers
	val vi: Vector[Int] = (1 to i).toVector
	// Now map each one to a BigInt, 'cause we'll need it!
	val vbi: Vector[BigInt] = vi.map( BigInt(_) )
	// Now we can do a sum…
	vbi.reduceLeft( _ + _ )
}