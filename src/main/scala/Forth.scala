import ForthError.{ForthError, UnknownWord}

import scala.annotation.tailrec

// udf word syntax:        : word def ;

case class State(stack: List[String]) extends ForthEvaluatorState:
   override def toString: String = stack.mkString(" ")

class Forth extends ForthEvaluator:
   def eval(text: String): Either[ForthError, ForthEvaluatorState] =
      val ws = text.toLowerCase.split(" ").toList
      run(ws).map(w => State(w.reverse))

@tailrec
def run(ws: List[String], st: List[String] = Nil): Either[ForthError, List[String]] =
   if ws.isEmpty then Right(st)
   else
      (ws.head, st) match
         case (":", _)              => Left(ForthError.UnknownWord)
         case ("+", a :: b :: tail) => run(ws.tail, (a.toInt + b.toInt).toString :: tail)
         case ("-", a :: b :: tail) => run(ws.tail, (b.toInt - a.toInt).toString :: tail)
         case ("*", a :: b :: tail) => run(ws.tail, (a.toInt * b.toInt).toString :: tail)
         case ("/", a :: b :: tail) =>
            if a == "0" then Left(ForthError.DivisionByZero)
            else
               run(
                 ws.tail,
                 (b.toInt / a.toInt).toString
                    :: tail)
         case (s, l) if s.matches("\\d+") => run(ws.tail, s :: l)
         case ("dup", a :: tail)          => run(ws.tail, a :: a :: tail)
         case ("drop", _ :: tail)         => run(ws.tail, tail)
         case ("swap", a :: b :: tail)    => run(ws.tail, b :: a :: tail)
         case ("over", a :: b :: tail)    => run(ws.tail, b :: a :: b :: tail)
         case (_, l) if l.size <= 1       => Left(ForthError.StackUnderflow)
         case (_, _)                      => Left(UnknownWord)
