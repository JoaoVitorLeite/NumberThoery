package numtheory

import Num.Implicits._

class NumberTheory[A](implicit op: Num[A]){

  def gcd(x:A, y: A): A = (x,y) match {
    case (a,b) if a < op.zero || b < op.zero => gcd(op.abs(a), op.abs(b))
    case _ => if(y == 0) x else gcd(y, x%y)
  }

  def lcm(x: A, y: A): A = (op.abs(x)*op.abs(y))/gcd(x,y)

  def isPrime(n: Int): Boolean = if (n == 1) false else !(2 until n).toList.exists(n%_ == 0)

  def nthPrime(x: Int): Int = {
    LazyList.from(1).filter(isPrime).slice(x-1,x).head
  }

  def primes(n: Int): List[Int] = LazyList.from(2).filter(isPrime).take(n).toList

  def factors(x: A):List[A] = {
    def foo(x: A, a: A): List[A] = x%a match {
      case _ if a * a > x => List(x)
      case 0 => a :: foo(x/a, a)
      case _ => foo(x, a + op.one)
    }
    foo(x, op.one+op.one)
  }

  def sieveOfErastosthenes(n: A): List[Int] = {
    List(2, 3, 5, 7) ::: LazyList.from(2).take((n-op.one).toINT).filterNot {
      x => x % 2 == 0 || x % 3 == 0 || x % 5 == 0 || x % 7 == 0
    }.toList
  }

  def sieveOfSundaram(n: A): List[Int] ={
    val k = (n/(op.one+op.one)).toINT
    val list = for{
      i <- 1 to k
      j <- i to (k-i)/(2*i+op.one.toINT)
      if i + j + 2 * i * j <= k
    }yield i+j+2*i*j
    2 :: ((1 to k).toList diff list map (2*_ + 1))
  }

  private def atkin(list: List[Int], limit: Int): List[Int] = {
    @scala.annotation.tailrec
    def foo(xs: List[Int], acc: List[Int]): List[Int] = xs match {
      case Nil => acc
      case hd::tl if hd <= limit && (hd % 12 == 1 || hd % 12 == 5) => foo(tl, hd::acc)
      case hd::tl if hd <= limit && hd % 12 == 7 => foo(tl, hd::acc)
      case hd::tl if hd <= limit && hd % 12 == 11 => foo(tl, hd::acc)
      case _::tl => foo(tl, acc)
    }
    foo(list, List())
  }

  def sieveOfAtkin(limit: A): List[Int] = {
    val res = 2::3::(for {
      i <- 1 until math.sqrt(limit.toINT).toInt
      j <- 1 until math.sqrt(limit.toINT).toInt
      a = (4 * i * i) + (j * j)
      b = (3 * i * i) + (j * j)
      c = (3 * i * i) - (j * j)
      res = if (i > j) atkin(List(a,b,c), limit.toINT) else atkin(List(a,b), limit.toINT)
    }yield res).toList.flatten
    res.filterNot{
      x =>
        val sqroot = math.sqrt(x)
        sqroot - math.floor(sqroot) == 0
    }.distinct
  }

  def multiples(n: Int, start: Int, end: Int): List[Int] = {
    if(start > end) Nil
    else{
      LazyList.from(start).take(end-start+1).filter(_ % n == 0).toList
    }
  }

  def phi(n: A): A = {
    @scala.annotation.tailrec
    def foo(x: A, acc: A): A = {
      if(x == 0) acc
      else
        if(gcd(n,x) == 1) foo(x - op.one, acc + op.one)
        else foo(x - op.one, acc)
    }
    foo(n, op.zero)
  }

  def extendEuclidianBezout(a: A, b: A): (A,A) = (a,b) match {
    case _ if a < op.zero || b < op.zero => extendEuclidianBezout(op.abs(a), op.abs(b))
    case (_, 0) => assume(a!=0);(op.one,op.zero)
    case _ =>
      val (x,y) = extendEuclidianBezout(b, a%b)
      (y, x-(a/b)*y)
  }

  def numberOfMultiples(n: A, a: A, b: A): Int = multiples(n.toINT, a.toINT, b.toINT).length

  def chineseRemainder(list: List[ChineseAux[A]]): A = {
    val numbers = list.map(_.number)
    val modulus = list.map(_.moduli)
    val N = modulus.foldLeft(op.one)((x,y) => x * y)
    val ni = modulus.map(N/_)
    val xi = (for{
      i <- list.indices
    }yield extendEuclidianBezout(ni(i), modulus(i))._1).toList
    val res = (for{
      i <- list.indices
    }yield numbers(i)*ni(i)*xi(i)).toList.foldLeft(op.zero)((x,y) => x+y)
    res % N
  }



}
