// Don't qualify as SAM type because result type is an implicit function type
trait Foo {
  def foo(x: Int): implicit Int => Int
}

class Test {
  val good = new Foo {
    def foo(x: Int) = 1
  }

  val bad: Foo = (x: Int) => 1 // error
}
