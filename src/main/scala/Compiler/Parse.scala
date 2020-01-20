package Prolog.Domain

import scala.meta._
import Prolog._
import Prolog.Domain.ADT.Term._
import Prolog.Domain.ADT.{Program, Query, Goal, Clause}

import scala.io.Source
import zio._

def parseQuery(str: String): Task[Query] = 
  IO.fromOption(
    str.parse[Term]
      .toOption
      .flatMap(metaToGoals)
      .map(gs => Query(gs)))
    .mapError(_ => Exception("Can't parse query."))

def parseTerm(str: String): Either[Throwable, ADT.Term] =
  str.parse[Term]
    .toEither.left.map(l => new Exception(s"Problem parsing scala term: $str"))
    .flatMap(m => metaToTerm(m).map(x => Right(x)).getOrElse(Left(new Exception(s"Problem parsing prolog term: $str"))))


def parsePredicate(str: String): Option[Predicate] =
  str.parse[Term]
    .toOption
    .flatMap(metaToTerm)
    .collect { case p: Predicate => p }

def parseProgram(lines: List[String]): Option[Program] =
  allOK(
    lines
      .filterNot(f => f.trim.startsWith("//"))
      .map(l => 
        l.parse[Term]
          .toOption
          .flatMap(metaToClause)))
      .map(c => c.foldLeft(Program())((a, b) => a.add(b)))

def metaToTerm(meta: Term): Option[Domain.ADT.Term] = 
  meta match
  case Term.Name(name: String) => name.headOption.map(h => if h.isUpper then Variable(name) else Atom(name))
  case Term.Apply(Term.Name(name), list) => allOK(list.map(m => metaToTerm(m))).map(o => Predicate(name, o))
  case t: Lit.Boolean => Some(Predicate("false"))
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

def metaToPredicate(meta: Term): Option[Predicate] = 
  metaToTerm(meta) match
  case Some(p: Predicate) => Some(p)
  case _ => None

def metaToClause(meta: Term): Option[Clause] = 
  for
    cm <- clauseMetaTerm(meta, Nil)
    ch <- metaToPredicate(cm._1)
    cb <- allOK(cm._2.map(m => metaToPredicate(m)))
  yield Clause(ch, cb)

def clauseMetaTerm(meta: Term, body: List[Term]): Option[(Term, List[Term])] = 
  //println(meta.structure)
  meta match
  case Term.ApplyInfix(h, Term.Name(":="), Nil, t1::Nil) => 
    clauseBodyMetaTerm(t1, Nil)
      .map(m => (h, m))
  case t: Term.Apply => Some((t, Nil))
  case _ => None

def clauseBodyMetaTerm(meta: Term, body: List[Term]): Option[List[Term]] =
  meta match
  case Term.ApplyInfix(t, Term.Name("=="), Nil, t1::Nil) => Some(Term.Apply(Term.Name("eql"), List(t, t1)) :: body)
  case Term.ApplyInfix(t, Term.Name("&&"), Nil, t1::Nil) => clauseBodyMetaTerm(t, t1 :: body)
  case t: Term.Apply => Some(t :: body)
  case t: Lit.Boolean => Some(t :: body)
  case _ => None

def metaToGoals(meta: Term): Option[List[Goal]] =
  clauseBodyMetaTerm(meta, Nil).map(m => m.map(x => metaToPredicate(x)).flatten)

def allOK[T](list: List[Option[T]]): Option[List[T]] = 
  if list.contains(None) then
    None 
  else 
    Some(list.collect { case Some(s) => s })