package Prolog.Domain

import scala.meta._
import Prolog._
import Prolog.Domain.Term._
import Prolog.Domain.Program

import scala.io.Source
import zio._

def parseQuery(str: String): Task[Query] = 
  IO.fromOption(
    metaToGoals(str.parse[Term].get)
      .map(gs => Query(gs)))
    .mapError(_ => Exception("Can't parse query."))

def parsePredicate(str: String): Option[Predicate] =
  metaToTerm(str.parse[Term].get) match
  case Some(p: Predicate) => Some(p)
  case _ => None

def parseProgram(lines: List[String]): Option[Program] =
  allOK(lines.map(l => metaToClause(l.parse[Term].get)))
    .map(c => c.foldLeft(Program())((a, b) => a.add(b)))

def metaToTerm(meta: Term): Option[Domain.Term] = 
  meta match
  case Term.Name(name: String) => name.headOption.map(h => if h.isUpper then Variable(name, 0) else Atom(name))
  case Term.Apply(Term.Name(name), list) => allOK(list.map(m => metaToTerm(m))).map(o => Predicate(name, o))
  case _ => None

def metaToPredicate(meta: Term): Option[Domain.Term.Predicate] = 
  metaToTerm(meta) match
  case Some(p: Predicate) => Some(p)
  case _ => None

def metaToClause(meta: Term): Option[Domain.Clause] = 
  for
    cm <- clauseMetaTerm(meta, Nil)
    ch <- metaToPredicate(cm._1)
    cb <- allOK(cm._2.map(m => metaToPredicate(m)))
  yield Domain.Clause(ch, cb)

def clauseMetaTerm(meta: Term, body: List[Term]): Option[(Term, List[Term])] = 
  meta match
  case Term.ApplyInfix(h, Term.Name(":-"), Nil, t1::Nil) => Some((h, t1 :: body))
  case Term.ApplyInfix(t, Term.Name("&&"), Nil, t1::Nil) => clauseMetaTerm(t, t1 :: body)
  case t: Term.Apply => Some((t, Nil))
  case _ => None

def metaToGoals(meta: Term): Option[List[Goal]] =
  goalsMetaTerm(meta, Nil).map(m => m.map(x => metaToPredicate(x)).flatten)

def goalsMetaTerm(meta: Term, body: List[Term]): Option[List[Term]] = 
  meta match
  case Term.ApplyInfix(t, Term.Name("&&"), Nil, t1::Nil) => goalsMetaTerm(t, t1 :: body)
  case t: Term.Apply => Some(t :: body)
  case _ => None

def allOK[T](list: List[Option[T]]): Option[List[T]] = 
  if list.contains(None) then
    None 
  else 
    Some(list.collect { case Some(s) => s })