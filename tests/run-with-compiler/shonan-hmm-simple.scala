import scala.quoted._

trait Ring[T] {
  val zero: T
  val one: T
  val add: (x: T, y: T) => T
  val sub: (x: T, y: T) => T
  val mul: (x: T, y: T) => T
}

class RingInt extends Ring[Int] {
  val zero = 0
  val one = 1
  val add = (x, y) => x + y
  val sub  = (x, y) => x - y
  val mul  = (x, y) => x * y
}

class RingIntExpr extends Ring[Expr[Int]] {
  val zero = '(0)
  val one = '(1)
  val add = (x, y) => '(~x + ~y)
  val sub  = (x, y) => '(~x - ~y)
  val mul  = (x, y) => '(~x * ~y)
}

class RingComplex[U](u: Ring[U]) extends Ring[Complex[U]] {
  val zero = Complex(u.zero, u.zero)
  val one  = Complex(u.one, u.zero)
  val add = (x, y) => Complex(u.add(x.re, y.re), u.add(x.im, y.im))
  val sub = (x, y) => Complex(u.sub(x.re, y.re), u.sub(x.im, y.im))
  val mul = (x, y) => Complex(u.sub(u.mul(x.re, y.re), u.mul(x.im, y.im)), u.add(u.mul(x.re, y.im), u.mul(x.im, y.re)))
}

sealed trait PV[T] {
  def expr(implicit l: Liftable[T]): Expr[T]
}
case class Sta[T](x: T) extends PV[T] {
  def expr(implicit l: Liftable[T]): Expr[T] = x.toExpr
}
case class Dyn[T](x: Expr[T]) extends PV[T] {
  def expr(implicit l: Liftable[T]): Expr[T] = x
}

case class RingPV[U: Liftable](staRing: Ring[U], dynRing: Ring[Expr[U]]) extends Ring[PV[U]] {
  type T = PV[U]

  import staRing._
  import dynRing._

  val zero: T = Sta(staRing.zero)
  val one: T = Sta(staRing.one)
  val add = (x: T, y: T) => (x, y) match {
    case (Sta(staRing.zero), x) => x
    case (x, Sta(staRing.zero)) => x
    case (Sta(x), Sta(y)) => Sta(staRing.add(x, y))
    case (x, y) => Dyn(dynRing.add(x.expr, y.expr))
  }
  val sub = (x: T, y: T) => (x, y) match {
    case (Sta(staRing.zero), x) => x
    case (x, Sta(staRing.zero)) => x
    case (Sta(x), Sta(y)) => Sta(staRing.sub(x, y))
    case (x, y) => Dyn(dynRing.sub(x.expr, y.expr))
  }
  val mul = (x: T, y: T) => (x, y) match {
    case (Sta(staRing.zero), _) => Sta(staRing.zero)
    case (_, Sta(staRing.zero)) => Sta(staRing.zero)
    case (Sta(staRing.one), x) => x
    case (x, Sta(staRing.one)) => x
    case (Sta(x), Sta(y)) => Sta(staRing.mul(x, y))
    case (x, y) => Dyn(dynRing.mul(x.expr, y.expr))
  }
}

case class Complex[T](re: T, im: T)

case class Vec[Idx, T](size: Idx, get: Idx => T) {
  def map[U](f: T => U): Vec[Idx, U] = Vec(size, i => f(get(i)))
  def zipWith[U, V](other: Vec[Idx, U], f: (T, U) => V): Vec[Idx, V] = Vec(size, i => f(get(i), other.get(i)))
}


trait VecOps[Idx, T] {
  val reduce: ((T, T) => T, T, Vec[Idx, T]) => T
}

class StaticVecOps[T] extends VecOps[Int, T] {
  val reduce: ((T, T) => T, T, Vec[Int, T]) => T = (plus, zero, vec) => {
    var sum = zero
    for (i <- 0 until vec.size)
      sum = plus(sum, vec.get(i))
    sum
  }
}

class StaticVecOptOps[T] extends VecOps[Int, T] {
  val reduce: ((T, T) => T, T, Vec[Int, T]) => T = (plus, zero, vec) => {
    var sum = zero
    for (i <- 0 until vec.size)
      sum = plus(sum, vec.get(i))
    sum
  }
}

class ExprVecOps[T: Type] extends VecOps[Expr[Int], Expr[T]] {
  val reduce: ((Expr[T], Expr[T]) => Expr[T], Expr[T], Vec[Expr[Int], Expr[T]]) => Expr[T] = (plus, zero, vec) => '{
    var sum = ~zero
    for (i <- 0 until ~vec.size)
      sum = ~{ plus('(sum), vec.get('(i))) }
    sum
  }
}

class Blas1[Idx, T](r: Ring[T], ops: VecOps[Idx, T]) {
  def dot(v1: Vec[Idx, T], v2: Vec[Idx, T]): T = ops.reduce(r.add, r.zero, v1.zipWith(v2, r.mul))
}

object Test {

  implicit val toolbox: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make

  def main(args: Array[String]): Unit = {
    val arr1 = Array(1, 2, 4, 8, 16)
    val arr2 = Array(1, 0, 1, 0, 1)

    val vec1 = new Vec(arr1.size, i => arr1(i))
    val vec2 = new Vec(arr2.size, i => arr2(i))
    val blasInt = new Blas1(new RingInt, new StaticVecOps)
    println(blasInt.dot(vec1, vec2))

    val vec3 = new Vec(arr1.size, i => Complex(2, arr2(i)))
    val vec4 = new Vec(arr2.size, i => Complex(1, arr2(i)))
    val blasComplexInt = new Blas1(new RingComplex(new RingInt), new StaticVecOps)
    println(blasComplexInt.dot(vec3, vec4))

    val vec5 = new Vec(5, i => arr1(i).toExpr)
    val vec6 = new Vec(5, i => arr2(i).toExpr)
    val blasStaticIntExpr = new Blas1(new RingIntExpr, new StaticVecOps)
    println(blasStaticIntExpr.dot(vec5, vec6).show)



    val code = '{
      val arr3 = Array(1, 2, 4, 8, 16)
      val arr4 = Array(1, 0, 1, 0, 1)
      ~{
        val vec7 = new Vec('(arr3.size), i => '(arr3(~i)))
        val vec8 = new Vec('(arr4.size), i => '(arr4(~i)))
        val blasExprIntExpr = new Blas1(new RingIntExpr, new ExprVecOps)
        blasExprIntExpr.dot(vec7, vec8)
      }

    }
    println(code.show)


    {
      val vec5 = new Vec[Int, PV[Int]](5, i => Dyn(arr1(i).toExpr))
      val vec6 = new Vec[Int, PV[Int]](5, i => Sta(arr2(i)))
      val blasStaticIntExpr = new Blas1(new RingPV[Int](new RingInt, new RingIntExpr), new StaticVecOps)
      blasStaticIntExpr.dot(vec5, vec6).expr.show
    }

    {
      val code = '{
        val arr3 = Array(1, 2, 4, 8, 16)
        ~{
          val vec7 = new Vec[Int, PV[Int]](5, i => Dyn('(arr3(~i.toExpr))))
          val vec8 = new Vec[Int, PV[Int]](5, i => Sta(arr2(i)))
          val blasExprIntExpr = new Blas1(new RingPV[Int](new RingInt, new RingIntExpr), new StaticVecOps)
          blasExprIntExpr.dot(vec7, vec8).expr
        }

      }
      println(code.show)
    }
  }

}
