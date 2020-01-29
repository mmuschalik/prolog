package Prolog.Domain

import scala.meta._
import Prolog._
import Prolog.Domain.ADT.Term._
import Prolog.Domain.ADT.{Program, Query, Goal, Clause}

import scala.io.Source
import zio._

def parseToMetaTerm(str: String): Either[Throwable, Term] =
  str.parse[Term]
    .toEither
    .left
    .map(_ => Exception(s"Invalid scala term: $str"))

def parseQuery(str: String): Task[Query] = 
  IO.fromEither(
    for
      metaTerm <- parseToMetaTerm(str)
      goals    <- metaTermToGoals(metaTerm)
    yield Query(goals))

def parseTerm(str: String): Either[Throwable, ADT.Term] =
  for 
    metaTerm <- parseToMetaTerm(str)
    term     <- metaTermToTerm(metaTerm)
  yield term

def metaTermToTerm(metaTerm: Term): Either[Throwable, ADT.Term] =
  metaToTerm(metaTerm)
    .fold(Left(Exception(s"Invalid term: ${metaTerm.toString}")))(Right(_))

def termToPredicate(term: ADT.Term): Either[Throwable, Predicate] =
  term match
  case p: Predicate => Right(p)
  case _ => Left(Exception(s"Term ${term.toString} is not a predicate"))

def parsePredicate(str: String): Either[Throwable, Predicate] =
  for 
    term       <- parseTerm(str)
    predicate  <- termToPredicate(term)
  yield predicate

def parseClause(str: String): Either[Throwable, Clause] =
  for 
    headBody <- parseClauseHead(str)
    headTerm <- metaTermToTerm(headBody._1)
    head     <- termToPredicate(headTerm)
    goals    <- headBody._2.fold(Right(Nil))(metaTermToGoals)
  yield Clause(head, goals)

def parseClauseHead(str: String): Either[Throwable, (Term, Option[Term])] =
  for
    wholeMetaTerm  <- parseToMetaTerm(str)
    clauseOption    = wholeMetaTerm match {
                        case Term.ApplyInfix(h, Term.Name(":="), Nil, b::Nil) =>
                          Some((h, Some(b)))
                        case t => None
                      }
  yield clauseOption.getOrElse((wholeMetaTerm, None))

def metaTermToGoals(metaTerm: Term): Either[Throwable, List[Goal]] =
  metaTerm match
  case Term.ApplyInfix(t, Term.Name("&&"), Nil, t1::Nil) => 
    for
      goal         <- metaTermToGoal(t1)
      goalRemainder = metaTermToGoals(t)
      result       <- goalRemainder.map(g => g ::: List(goal))
    yield result
  case t => metaTermToGoal(t).map(m => m :: Nil)
  
def metaTermToGoal(metaTerm: Term): Either[Throwable, Goal] =
  for
    term <- metaTermToTerm(metaTerm)
    goal <- termToPredicate(term)
  yield goal

case class AggException(list: List[Throwable]) extends Throwable

def parseProgram(lines: List[String]): Either[Throwable, Program] =
  val clauseEithers = lines
                        .filterNot(f => f.trim.startsWith("//"))
                        .map(parseClause)
  
  val failures  = clauseEithers
                    .collect {case Left(l) => l}
                    .toList
  
  if failures.isEmpty then
    Right(clauseEithers
      .collect{case Right(r) => r}
      .foldLeft(Program())((a, b) => a.add(b)))
  else
    Left(AggException(failures))

def metaToTerm(meta: Term): Option[Domain.ADT.Term] = 
  meta match
  case Term.Name(name: String) => 
    name.headOption
      .map(h => if h.isUpper then Variable(name) else Atom(name))
  case Term.Apply(Term.Name(name), list) => 
    allOK(list.map(m => metaToTerm(m)))
      .map(o => Predicate(name, o))
  case t: Lit.Boolean => Some(Predicate("false"))
  case str: Lit.String => Some(Atom(str.value))
  case Term.ApplyInfix(t, Term.Name("=="), Nil, t1::Nil) => 
    for
      l <- metaToTerm(t)
      r <- metaToTerm(t1)
    yield Predicate("eql", l :: r :: Nil)
  case Term.ApplyInfix(t, Term.Name("!="), Nil, t1::Nil) => 
    for
      l <- metaToTerm(t)
      r <- metaToTerm(t1)
    yield Predicate("not", Predicate("eql", l :: r :: Nil) :: Nil)
  case _ => None

def allOK[T](list: List[Option[T]]): Option[List[T]] = 
  if list.contains(None) then
    None 
  else 
    Some(list.collect { case Some(s) => s })