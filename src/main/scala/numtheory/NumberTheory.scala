package numtheory

import Num.Implicits._

/***
 * @author João Vitor Leite
 */

/***
 * Class with some concepts of Number Theory
 * @param op Implicit of type class
 * @tparam A Type class
 */
class NumberTheory[A](implicit op: Num[A]){

  /***
   * Method for calculating the greatest common divisor of two numbers
   * @param x A number
   * @param y A number
   * @return The greatest common divisor of two numbers
   */
  def gcd(x:A, y: A): A = (x,y) match {
    case (a,b) if a < op.zero || b < op.zero => gcd(op.abs(a), op.abs(b))
    case _ => if(y == 0) x else gcd(y, x%y)
  }

  /***
   * Method for calculating the least common multiple of two numbers
   * @param x A number
   * @param y A number
   * @return The least common multiple
   */
  def lcm(x: A, y: A): A = (op.abs(x)*op.abs(y))/gcd(x,y)

  /***
   * Method to check if a number is prime
   * @param n A number
   * @return Return true if the number is prime, otherwise false
   */
  def isPrime(n: Int): Boolean = if (n == 1) false else !(2 until n).toList.exists(n%_ == 0)

  /***
   * Method for calculating the nth prime number
   * @param x nth prime number
   * @return The nth prime number
   */
  def nthPrime(x: Int): Int = {
    LazyList.from(1).filter(isPrime).slice(x-1,x).head
  }

  /***
   * Method for finding prime numbers
   * @param n Number of prime numbers
   * @return List of prime numbers
   */
  def primes(n: Int): List[Int] = LazyList.from(2).filter(isPrime).take(n).toList

  /**
   * Method for calculating the prime factors of a number
   * @param x A number
   * @return List of prime factors of a number `x`
   */
  def factors(x: A):List[A] = {
    def foo(x: A, a: A): List[A] = x%a match {
      case _ if a * a > x => List(x)
      case 0 => a :: foo(x/a, a)
      case _ => foo(x, a + op.one)
    }
    foo(x, op.one+op.one)
  }

  /***
   * Sieve of Erastosthenes
   *
   * <p>
   *   More: https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
   * </p>
   *
   * @param n Prime numbers to `n`
   * @return List of prime numbers
   */
  def sieveOfErastosthenes(n: A): List[Int] = {
    List(2, 3, 5, 7) ::: LazyList.from(2).take((n-op.one).toINT).filterNot {
      x => x % 2 == 0 || x % 3 == 0 || x % 5 == 0 || x % 7 == 0
    }.toList
  }

  /***
   * Sieve of Sundaram
   *
   * <p>
   *   More: https://en.wikipedia.org/wiki/Sieve_of_Sundaram
   * </p>
   *
   * @param n Prime numbers to `n`
   * @return List of prime numbers
   */
  def sieveOfSundaram(n: A): List[Int] ={
    val k = (n/(op.one+op.one)).toINT
    val list = for{
      i <- 1 to k
      j <- i to (k-i)/(2*i+op.one.toINT)
      if i + j + 2 * i * j <= k
    }yield i+j+2*i*j
    2 :: ((1 to k).toList diff list map (2*_ + 1))
  }

  /***
   * Auxiliary method for the Sieve of Atkin
   * @param list List of numbers to be evaluated
   * @param limit Prime number to `limit`
   * @return List of numbers after being evaluated
   */
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

  /***
   * Sieve of Atkin
   *
   * <p>
   *   More: https://en.wikipedia.org/wiki/Sieve_of_Atkin
   *   Obs.: List of numbers out of sequence
   * </p>
   *
   * @param limit Prime number to `limit`
   * @return List of prime numbers
   */
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

  /***
   * Multiples of a number in a given interval
   * @param n A number
   * @param start Start of the range
   * @param end End of the range
   * @return List of multiples of a number `n`
   */
  def multiples(n: Int, start: Int, end: Int): List[Int] = {
    if(start > end) Nil
    else{
      LazyList.from(start).take(end-start+1).filter(_ % n == 0).toList
    }
  }

  /***
   * Euler's Totient Function
   *
   * <p>
   *   More: https://en.wikipedia.org/wiki/Euler%27s_totient_function
   * </p>
   *
   * @param n A number to apply the totient function
   * @return Number of numbers less than or equal to `n` that are co-prime with it
   */
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

  /***
   * Extend Euclidian Algorithm
   *
   * <p>
   *   More: Solve a*x + b*y = gcd(a,b) for (x,y)
   * </p>
   *
   * @param a A number
   * @param b A number
   * @return (x,y) that is the result of a*x + b*y = gcd(x,y)
   */
  def extendEuclidianBezout(a: A, b: A): (A,A) = (a,b) match {
    case _ if a < op.zero || b < op.zero => extendEuclidianBezout(op.abs(a), op.abs(b))
    case (_, 0) => assume(a!=0);(op.one,op.zero)
    case _ =>
      val (x,y) = extendEuclidianBezout(b, a%b)
      (y, x-(a/b)*y)
  }

  /***
   * Number of multiples of `n` in a given interval
   * @param n A number
   * @param a Start of the range
   * @param b End of the range
   * @return Number of multiples of `n` in a given interval
   */
  def numberOfMultiples(n: A, a: A, b: A): Int = multiples(n.toINT, a.toINT, b.toINT).length

  /***
   * Chinese Remaninder(see the class ChineseAux)
   * @param list List of element ChineseAux
   * @return Result of the application of the Chinese Remainder Theorem
   */
  def chineseRemainder(list: List[ChineseAux[A]]): A = {
    val numbers = list.map(_.number)
    val modulus = list.map(_.modulus)
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
