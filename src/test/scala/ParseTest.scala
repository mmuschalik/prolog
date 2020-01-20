package Prolog.Domain

import org.junit._
import Prolog.Domain.{parseProgram}
import Prolog.Domain.ADT._
import Prolog.Domain.ADT.Term._
import Assert._

class ParseTest {

  @Test
  def testParseSimpleFact: Unit =
    val line = "test(A, b)"
    val p = parsePredicate(line)
    assertEquals("Test parsing a simple fact", Some(new Predicate("test", Variable("A") :: Atom("b") :: Nil)), p)

  @Test
  def testParseNestedPredicate: Unit =
    val line = "test(not(A))"
    val p = parsePredicate(line)
    assertEquals("Test parsing a simple fact", Some(new Predicate("test", new Predicate("not", Variable("A") :: Nil) :: Nil)), p)

  @Test
  def testEqualityPredicate: Unit =
    val line = "A == B"
    val p = parseTerm(line)
    assertEquals("Test parsing equality", Right(new Predicate("eql", Variable("A") :: Variable("B") :: Nil)), p)

}