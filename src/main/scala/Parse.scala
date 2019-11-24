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

def metaToClause(meta: Term): Option[Domain.Clause] = {
  None
}

def allOK[T](list: List[Option[T]]): Option[List[T]] = if (list.contains(None)) None else Some(list.collect{ case Some(s) => s })