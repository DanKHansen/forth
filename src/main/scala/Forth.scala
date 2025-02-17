import ForthError.ForthError

import scala.annotation.tailrec

case class State(stack: List[Int] = List.empty) extends ForthEvaluatorState:
   def push(value: Int): State = copy(stack = value :: stack)
   def pop(): Either[ForthError.Value, (Int, State)] =
      stack match
         case ::(head, next) => Right((head, copy(stack = next)))
         case Nil            => Left(ForthError.StackUnderflow)
   override def toString: String = super.toString

class Forth extends ForthEvaluator:
   def eval(text: String): Either[ForthError, ForthEvaluatorState] =
      val words = text.split(" ").toList
      @tailrec
      def execute(ws: List[String], st: List[Int]): Either[ForthError, ForthEvaluatorState] =
         if ws.isEmpty then Right(State(st))
         else
            ws match
               case t :: r if t.forall(_.isDigit) => execute(r, State(st).push(t.toInt).stack)
               // case _                             => Left(ForthError.UnknownWord)
      execute(words, Nil)
