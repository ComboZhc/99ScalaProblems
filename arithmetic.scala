package arithmetic

import language.implicitConversions

class S99Int(val t: Int) {
    import S99Int._
    // P31
    def isPrime: Boolean = 
        (t > 1) && (primes.takeWhile{_ <= Math.sqrt(t)}.forall{t % _ != 0})

    // P33
    def isCoprimeTo(o: Int): Boolean = gcd(t, o) == 1

    // P34
    def totient: Int = (1 to t).count{_.isCoprimeTo(t)}

    // P36
    def primeFactorMultiplicity: List[(Int, Int)] = {
        def factorCount(n: Int, p: Int): Int = 
            if (n % p != 0) 0 else 1 + factorCount(n / p, p)
        def factorsR(n: Int, ps: Stream[Int]): List[(Int, Int)] = 
            if (ps.isEmpty) Nil else (ps.head, factorCount(n, ps.head)) :: factorsR(n, ps.tail)
        factorsR(t, primes.takeWhile{_ <= t}.filter{t % _ == 0})
    }

    // P35
    def primeFactors: List[Int] = t.primeFactorMultiplicity.flatMap(e => List.fill(e._2){e._1})

    // P37
    def totient2: Int = t.primeFactorMultiplicity.foldLeft(1) { (r, f) =>
        f match { case (p, m) => r * (p - 1) * Math.pow(p, m - 1).toInt }
    }

    def goldbach: (Int,Int) =
        primes.takeWhile{ _ < t }.find{ p => (t - p).isPrime } match {
            case None     => throw new IllegalArgumentException
            case Some(p1) => (p1, t - p1)
        }
}

object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    // P31
    val primes = Stream.cons(2, Stream.from(3, 2).filter{_.isPrime})
    
    // P32
    def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    // P39
    def listPrimesinRange(r: Range): List[Int] =
        primes.dropWhile{ _ < r.start }.takeWhile{ _ <= r.last }.toList

    def printGoldbachList(r: Range) {
        printGoldbachListLimited(r, 0)
    }

    def printGoldbachListLimited(r: Range, limit: Int) {
        (r filter { n => n > 2 && n % 2 == 0 } 
            map { n => (n, n.goldbach) }
            filter { _._2._1 >= limit }
            foreach {
                _ match { case (n, (p1, p2)) => println(n + " = " + p1 + " + " + p2) }
            })
    }
}