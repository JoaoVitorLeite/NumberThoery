package numtheory

import org.scalatest.FunSuite

class NumberTheorySuite extends FunSuite{

  test("T1"){
    val a = new NumberTheory[Int]
    assert(a.gcd(5,-9) == 1)
  }
  test("T2"){
    val a = new NumberTheory[Int]
    assert(a.gcd(5,10) == 5)
  }
  test("T3"){
    val a = new NumberTheory[Int]
    assert(a.gcd(-5,-10) == 5)
  }
  test("T4"){
    val a = new NumberTheory[Int]
    assert(a.lcm(5, 10) == 10)
  }
  test("T5"){
    val a = new NumberTheory[Int]
    assert(a.lcm(-5,10) == 10)
  }
  test("T6"){
    val a = new NumberTheory[Int]
    assert(a.isPrime(2))
  }
  test("T7"){
    val a = new NumberTheory[Int]
    assert(!a.isPrime(12))
  }
  test("T8"){
    val a = new NumberTheory[Int]
    val b = List(1, 2, 5, 7, 6, 12, 18)
    assert(b.map(a.isPrime) == List[Boolean](false,true,true,true,false,false,false))
  }
  test("T9"){
    val a = new NumberTheory[Int]
    assert(a.nthPrime(12) == 37)
  }
  test("T10"){
    val a = new NumberTheory[Int]
    assert(a.nthPrime(39) == 167)
  }
  test("T11"){
    val a = new NumberTheory[Int]
    assert(a.primes(10) == List(2,3,5,7,11,13,17,19,23,29))
  }
  test("T12"){
    val a = new NumberTheory[Int]
    assert(a.primes(12) == List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37))
  }
  test("T13"){
    val a = new NumberTheory[Int]
    assert(a.factors(30) == List(2,3,5))
  }
  test("T14"){
    val a = new NumberTheory[Int]
    assert(a.sieveOfErastosthenes(20) == List(2, 3, 5, 7, 11, 13, 17, 19))
  }
  test("T15"){
    val a = new NumberTheory[Int]
    assert(a.sieveOfSundaram(20) == List(2, 3, 5, 7, 11, 13, 17, 19))
  }
  test("T16"){
    val a = new NumberTheory[Int]
    assert(a.sieveOfAtkin(20) == List(2, 3, 5, 7, 13, 11, 17))
  }
  test("T17"){
    val a = new NumberTheory[Int]
    assert(a.multiples(5,-1,20) == List(0,5,10,15,20))
  }
  test("T18"){
    val a = new NumberTheory[Int]
    assert(a.phi(9) == 6)
  }
  test("T19"){
    val a = new NumberTheory[Int]
    assert(a.extendEuclidianBezout(10,5) == (0,1))
  }
  test("T20"){
    val a = new NumberTheory[Int]
    assert(a.numberOfMultiples(5,-1,20) == 5)
  }



}
