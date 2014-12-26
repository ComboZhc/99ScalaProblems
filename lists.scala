def last[A](list: List[A]): A = list match {
    case head :: Nil => head
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
}

def penultimate[A](list: List[A]): A = list match {
    case head :: _ :: Nil => head
    case _ :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
}

def nth[A](n: Int, list:List[A]): A = (n, list) match {
    case (0, head :: _) => head
    case (n, _ :: tail) => nth(n - 1, tail)
    case (_, Nil) => throw new NoSuchElementException
}

def length[A](list: List[A]): Int = list match {
    case Nil => 0
    case _ :: tail => 1 + length(tail)
}

// Tail Recursive
def reverse[A](list: List[A]): List[A] = {
    def reverseR(result: List[A], cur: List[A]): List[A] = cur match {
        case Nil => result
        case head :: tail => reverseR(head :: result, tail) 
    }
    reverseR(Nil, list)        
}

def isPalindrome[A](list: List[A]): Boolean = {
    reverse(list) == list
}

def flatten(list: List[Any]): List[Any] = list.flatMap {
    case l: List[Any] => flatten(l)
    case e => List(e)
}

def compress[A](list: List[A]): List[A] = list.foldRight(List[A]()) {
    (h, r) => if (!r.isEmpty && r.head == h) r else h :: r
}

def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty)
        List()
    else {
        val (packed, next) = ls.span(_ == ls.head)
        if (next == Nil)
            List(packed)
        else
            packed :: pack(next)
    }
}

def encode[A](ls: List[A]): List[(Int, A)] = {
    pack(ls).map(x => (x.size, x.head))
}

def encodeModified[A](ls: List[A]): List[Any] = {
    encode(ls).map(x => if (x._1 > 1) x else x._2)
}

def decode[A](ls: List[(Int, A)]): List[A] = {
    ls.flatMap(x => List.fill(x._1){x._2})
}

def encodeDirect[A](ls: List[A]): List[(Int, A)] = {
    if (ls.isEmpty)
        List()
    else {
        val (packed, next) = ls.span(_ == ls.head)
        if (next == Nil)
            List((packed.size, packed.head))
        else
            (packed.size, packed.head) :: encodeDirect(next)
    }
}

def duplicate[A](ls: List[A]): List[A] = ls.flatMap(e => List(e, e))

def duplicateN[A](n: Int, ls: List[A]): List[A] = ls.flatMap(e => List.fill(n){e})

def drop[A](n: Int, ls: List[A]): List[A] = {
    def dropR[A](i: Int, cur: List[A]): List[A] = (i, cur) match {
        case (_, Nil) => Nil
        case (1, head :: tail) => dropR(n, tail)
        case (c, head :: tail) => head :: dropR(c - 1, tail)
    }
    dropR(n, ls)
}

def split[A](n: Int, ls: List[A]): (List[A], List[A]) = (n, ls) match {
    case (_, Nil) => (Nil, Nil)
    case (0, l) => (Nil, l)
    case (i, h :: t) => {
        var (a, b) = split(i - 1, t)
        (h :: a, b)
    } 
}

def slice[A](i: Int, k: Int, ls: List[A]): List[A] = (i, k, ls) match {
    case (_, _, Nil) => Nil
    case (_, e, _) if e <= 0=> Nil
    case (s, e, h :: t) if s <= 0 => h :: slice(s - 1, e - 1, t)
    case (s, e, _ :: t) => slice(s - 1, e - 1, t)
}

def rotate[A](n: Int, ls: List[A]): List[A] = {
    val nBounded = if (ls.isEmpty) 0 else n % ls.length
    if (nBounded < 0)
        rotate(nBounded + ls.length, ls)
    else
        ls.drop(nBounded) ::: ls.take(nBounded)
}

def removeAt[A](n: Int, ls: List[A]): (List[A], A) = ls.splitAt(n) match {
    case (Nil, _) if n < 0 => throw new NoSuchElementException
    case (pre, e :: post)  => (pre ::: post, e)
    case (pre, Nil)        => throw new NoSuchElementException
}

def insertAt[A](e: A, n: Int, ls: List[A]): List[A] = ls.splitAt(n) match {
    case (pre, post) => pre ::: e :: post
}

def range(s: Int, e: Int): List[Int] = {
    if (s > e) Nil else s :: range(s + 1, e)
}

def randomSelect[A](n: Int, ls: List[A]): List[A] = {
    def randomSelectR(n: Int, ls: List[A], r: util.Random): List[A] =
        if (n == 0) Nil else {
            var (l, e) = removeAt(r.nextInt(ls.length), ls)
            e :: randomSelectR(n - 1, l, r)
        }
    randomSelectR(n, ls, new util.Random)
}

def lotto(n: Int, m: Int): List[Int] = randomSelect(n, range(1, m))

def randomPermute[A](ls: List[A]): List[A] = randomSelect(ls.length, ls)

def combinations[A](n: Int, ls: List[A]): List[List[A]] = {
    def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] = ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl => combinations(n - 1, sl.tail).map{sl.head :: _} }
}

def group[A](ns: List[Int], ls: List[A]): List[List[List[A]]] = ns match {
    case Nil     => List(Nil)
    case n :: ns => combinations(n, ls).flatMap{ c => group(ns, ls.diff(c)).map{c :: _} }
}

def lsort[A](ls: List[List[A]]): List[List[A]] = ls.sortBy(_.length)

def lsortFreq[A](ls: List[List[A]]): List[List[A]] = {
    val freqs = ls.map(_.length).groupBy(identity).mapValues(_.length)
    ls.sortBy(x => freqs(x.length))
}