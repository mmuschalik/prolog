package Prolog.Domain

import org.junit._
import Prolog.Domain.{parseProgram, parsePredicate, parseClause}
import Prolog.Domain.ADT._
import Prolog.Domain.ADT.Term._
import Assert._

class ParseTest {

  @Test
  def simpleStringAtom: Unit =
    val line = """test("a b")"""
    val p = parsePredicate(line)
    assertEquals("Test parsing a simple string atom",
      Right(new Predicate("test", Atom("a b") :: Nil)), 
      p)

  @Test
  def testParseSimpleFact: Unit =
    val line = "test(A, b)"
    val p = parsePredicate(line)
    assertEquals("Test parsing a simple predicate", 
      Right(new Predicate("test", Variable("A") :: Atom("b") :: Nil)), 
      p)

  @Test
  def testParseNestedPredicate: Unit =
    val line = "test(not(A))"
    val p = parsePredicate(line)
    assertEquals("Test parsing a nested predicate", Right(new Predicate("test", new Predicate("not", Variable("A") :: Nil) :: Nil)), p)

  @Test
  def testParseEqualityPredicate: Unit =
    val line = "A == B"
    val p = parsePredicate(line)
    assertEquals("Test parsing equality", 
      Right(new Predicate("eql", Variable("A") :: Variable("B") :: Nil)), 
      p)

  @Test
  def testFact: Unit =
    val line = "test(A,B)"
    val p = parseClause(line)
    assertEquals("Test parsing facts", 
      Right(Clause(new Predicate("test", Variable("A") :: Variable("B") :: Nil))), 
      p)
  
  @Test
  def testClauseOneGoalBody: Unit =
    val line = "test(A) := g1(A)"
    val p = parseClause(line)
    assertEquals("Test clause with one goal in body", 
      Right(Clause(new Predicate("test", Variable("A") :: Nil), new Predicate("g1", Variable("A") :: Nil) :: Nil)), 
      p)

  @Test
  def testClauseManyGoalBody: Unit =
    val line = "test(A) := g1(A) && g2(A)"
    val p = parseClause(line)
    assertEquals("Test clause with one goal in body",
      Right(Clause(
        new Predicate("test", Variable("A") :: Nil), 
          new Predicate("g1", Variable("A") :: Nil) ::
          new Predicate("g2", Variable("A") :: Nil) 
          :: Nil)), 
      p)
  
}