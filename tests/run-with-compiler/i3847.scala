import dotty.tools.dotc.quoted.Toolbox._
import scala.quoted._
import scala.reflect.ClassTag

object Arrays {
  implicit def ArrayIsLiftable[T: Liftable](implicit t: Type[T], ct: Expr[ClassTag[T]]): Liftable[Array[T]] = (arr: Array[T]) => '{
    new Array[~t](~arr.length.toExpr)(~ct)
    // TODO add elements
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    import Arrays._
    implicit val ct: Expr[ClassTag[Int]] = '(ClassTag.Int)
    val arr: Expr[Array[Int]] = Array[Int](1, 2, 3).toExpr
    println(arr.show)
  }
}