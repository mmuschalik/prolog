package Prolog.Domain

import Prolog.Domain.Operation.{stackShow, queryShow, show, unify, renameVariables, mergeBindings, substitute}
import Prolog.Domain.Operation._
import Prolog.Domain.ADT._
import Prolog.Domain.ADT.Term._
import zio.stream.Stream

import collection.JavaConverters._

type JList = java.util.List
type JMap = java.util.Map

def eval(facts: JMap[String, JList[JList[String]]], rules: JList[String], query: String): JList[JMap[String,String]] =
  val program = rulesToClause(rules).foldLeft(buildFacts(facts))((p, c) => p.add(c))
  val resultStream = queryProgram(program, singleQuery(query))

  val solutionZio = resultStream.map(r => 
    r._2
      .map(bs => 
        bs.map(b => (b._1.toString, b._2.toString)).toMap)
      .toList
    ).runCollect
    
  import zio._
  val res = new DefaultRuntime {}.unsafeRun(solutionZio)
  
  asJava(res.flatten.map(f => asJava(f)))

def rulesToClause(rules: JList[String]): List[Clause] =
  asScala(rules).map(m => parseClause(m).right.get).toList

def singleQuery(str: String): Query =
  Query(parseClause(str).right.get.head :: Nil)

def buildFacts(facts: JMap[String, JList[JList[String]]]): Program =
  asScala(facts)
  .flatMap(m => asScala(m._2)
    .map(x => Clause(new Predicate(m._1, asScala(x)
      .map(c => Atom(c)).toList))).toList)
  .foldLeft(Program())((p, c) => p.add(c))
  