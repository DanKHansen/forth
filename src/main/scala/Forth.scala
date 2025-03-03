import ForthError.ForthError

import scala.annotation.tailrec

type Result = Either[ForthError, ForthEvaluatorState]

case class State(stack: List[Int]) extends ForthEvaluatorState:
   override def toString: String = super.toString

class Forth extends ForthEvaluator:
   def eval(text: String): Result =
      val words = text.toLowerCase.split(" ").toList
      execute(words, Right(State(Nil)))

object Add extends Definition:
   override def evaluate(state: Result): Result =
      state match
         case Left(value) => Left(value)
         case Right(st)   =>
            st.stack match
               case l if l.length < 2      => Left(ForthError.StackUnderflow)
               case head :: follow :: next => Right(State((head + follow) :: next))
               case _                      => Left(ForthError.InvalidWord)

object Sub extends Definition:
   override def evaluate(state: Result): Result =
      state match
         case Left(value) => Left(value)
         case Right(st)   =>
            st.stack match
               case l if l.length < 2      => Left(ForthError.StackUnderflow)
               case head :: follow :: next => Right(State((follow - head) :: next))
               case _                      => Left(ForthError.InvalidWord)

object Mul extends Definition:
   override def evaluate(state: Result): Result =
      state match
         case Left(value) => Left(value)
         case Right(st)   =>
            st.stack match
               case l if l.length < 2      => Left(ForthError.StackUnderflow)
               case head :: follow :: next => Right(State((head * follow) :: next))
               case _                      => Left(ForthError.InvalidWord)

object Div extends Definition:
   override def evaluate(state: Result): Result =
      state match
         case Left(value) => Left(value)
         case Right(st)   =>
            st.stack match
               case 0 :: _                 => Left(ForthError.DivisionByZero)
               case head :: follow :: next => Right(State((follow / head) :: next))
               case l if l.length < 2      => Left(ForthError.StackUnderflow)
               case _                      => Left(ForthError.InvalidWord)

object Dup extends Definition:
   override def evaluate(state: Result): Result =
      state match
         case Left(value) => Left(value)
         case Right(st)   =>
            st.stack match
               case l if l.length < 1 => Left(ForthError.StackUnderflow)
               case head :: next      => Right(State(head :: head :: next))
               case _                 => Left(ForthError.InvalidWord)

object Swap extends Definition:
   override def evaluate(state: Result): Result =
      state match
         case Left(value) => Left(value)
         case Right(st)   =>
            st.stack match
               case l if l.length < 1      => Left(ForthError.StackUnderflow)
               case head :: follow :: next => Right(State(follow :: head :: next))
               case _                      => Left(ForthError.InvalidWord)

object Over extends Definition:
   override def evaluate(state: Result): Result =
      state match
         case Left(value) => Left(value)
         case Right(st)   =>
            st.stack match
               case l if l.length < 1      => Left(ForthError.StackUnderflow)
               case head :: follow :: next => Right(State(follow :: head :: follow :: next))
               case _                      => Left(ForthError.InvalidWord)

object Drop extends Definition:
   override def evaluate(state: Result): Result =
      state match
         case Left(value) => Left(value)
         case Right(st)   =>
            st.stack match
               case l if l.length < 1 => Left(ForthError.StackUnderflow)
               case _ :: next         => Right(State(next))
               case _                 => Left(ForthError.InvalidWord)

case class Num(num: Int) extends Definition:
   override def evaluate(state: Result): Result =
      state match
         case Left(value) => Left(value)
         case Right(st)   => Right(State(num :: st.stack))

def Udf(l: List[String]): Result =
   val ws = l.splitAt(l.indexOf(";"))
   val wrd = ws._1.tail
   val rest = wrd.tail
   println(ws)
   println(wrd)
   println(rest)
   execute(rest, Right(State(Nil)))

@tailrec
def execute(ws: List[String], st: Result): Result =
   // println(ws)
   if ws.isEmpty then st
   else
      ws match
         case num :: tail if num.forall(_.isDigit) => execute(tail, Num(num.toInt).evaluate(st))
         case ::(head, next) if head == "+"        => execute(next, Add.evaluate(st))
         case ::(head, next) if head == "-"        => execute(next, Sub.evaluate(st))
         case ::(head, next) if head == "*"        => execute(next, Mul.evaluate(st))
         case ::(head, next) if head == "/"        => execute(next, Div.evaluate(st))
         case ::(head, next) if head == "dup"      => execute(next, Dup.evaluate(st))
         case ::(head, next) if head == "drop"     => execute(next, Drop.evaluate(st))
         case ::(head, next) if head == "swap"     => execute(next, Swap.evaluate(st))
         case ::(head, next) if head == "over"     => execute(next, Over.evaluate(st))
         case ::(head, _) if head == ":"           => Udf(ws)
         case _                                    => Left(ForthError.InvalidWord)
