package cont

import org.scalatest.FunSuite

class Suite extends FunSuite {

  test("ambiguous") {
    def amb[A, B](xs: A*) = shift((k: A => List[B]) => xs.toList.flatMap(k))
    val example = reset {
      val x = amb(1, 3)
      val y = amb(2, 4)
      List(x * y)
    }
    assert(example == List(2, 4, 6, 12))
  }

  test("generator") {
    def gen[A](x: A) = shift((k: Unit => List[A]) => x :: k(()))
    val example =
      reset {
	gen(0)
	gen(1)
	gen(2)
	List[Int]()
      }
    assert(example == List(0, 1, 2))
  }

  test("stack") {
    def push[A, B](x: A) = shift((k: Unit => List[A] => B) => (xs: List[A]) => k(())(x :: xs))
    def pop[A, B] = shift((k: Option[A] => List[A] => B) => (xs: List[A]) => k(xs.headOption)(xs.drop(1)))
    var out: Any = null
    val example =
      reset {
	push(1)
	push(2)
	pop[Int, List[Int]]
	push(3)
        (xs: List[Int]) => xs
      }
    assert(example(Nil) == List(3, 1))
  }

}
