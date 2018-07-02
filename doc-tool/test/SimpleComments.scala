package dotty.tools
package dottydoc

import dotc.util.SourceFile

import org.junit.Test
import org.junit.Assert._

class SimpleCommentsFromSourceTest extends SimpleCommentsBase with CheckFromSource
class SimpleCommentsFromTastyTest extends SimpleCommentsBase with CheckFromTasty

abstract class SimpleCommentsBase extends DottyDocTest {

  @Test def simpleComment = {
    val source = new SourceFile(
      "HelloWorld.scala",
      """
      |package scala
      |
      |/** Hello, world! */
      |trait HelloWorld
      """.stripMargin
    )

    val className = "scala.HelloWorld"

    check(className :: Nil, source :: Nil) { (ctx, packages) =>
      val traitCmt =
        packages("scala")
        .children.find(_.path.mkString(".") == "scala.HelloWorld")
        .flatMap(_.comment.map(_.body))
        .get

      assertEquals(traitCmt, "<p>Hello, world!</p>")
    }
  }
}
