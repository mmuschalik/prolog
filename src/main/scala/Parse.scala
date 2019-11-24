package Prolog

import scala.meta._
import Prolog._
import Prolog.Domain.Term._
import Prolog.Domain.Program

def parseT[T](str: String): Option[T] = {
  metaToTerm(str.parse[Term].get) match {
    case Some(p: T) => Some(p)
    case _ => None
  }
}

def parseProgram(lines: List[String]): Option[Program] = {
  allOK(lines.map(l => metaToClause(l.parse[Term].get)))
    .map(c => c.foldLeft(Program())((a,b) => a.add(b)))
}

def metaToTerm(meta: Term): Option[Domain.Term] = meta match {
  case Term.Name(name: String) => name.headOption.map(h => if (h.isUpper) Variable(name) else Atom(name))
  case Term.Apply(Term.Name(name), list) => allOK(list.map(m => metaToTerm(m))).map(o => Predicate(name, o))
  case _ => None
}

def metaToPredicate(meta: Term): Option[Domain.Term.Predicate] = 
  metaToTerm(meta) match {
    case Some(p: Predicate) => Some(p)
    case _ => None
  }

def metaToClause(meta: Term): Option[Domain.Clause] = for {
  cm <- clauseMetaTerm(meta, Nil)
  ch <- metaToPredicate(cm._1)
  cb <- allOK(cm._2.map(m => metaToTerm(m)))
} yield Domain.Clause(ch, cb)

def clauseMetaTerm(meta: Term, body: List[Term]): Option[(Term, List[Term])] = meta match {
  case Term.ApplyInfix(h, Term.Name(":-"), Nil, t1::Nil) => Some((h, body))
  case Term.ApplyInfix(t, Term.Name("&&"), Nil, t1::Nil) => clauseMetaTerm(t, t1 :: body)
  case t: Term.Apply => Some((t, Nil))
  case _ => None
}

def allOK[T](list: List[Option[T]]): Option[List[T]] = if (list.contains(None)) None else Some(list.collect{ case Some(s) => s })