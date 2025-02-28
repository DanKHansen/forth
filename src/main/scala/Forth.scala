import ForthError.ForthError

import scala.annotation.tailrec

type Result = Either[ForthError, ForthEvaluatorState]

case class State(stack: List[Int] = List.empty) extends ForthEvaluatorState:
   override def toString: String = super.toString

case class Def(word: String, definition: List[String], sta: State) extends Definition:
   private val udf: Map[String, List[String]] = Map("dup-twice" -> List("dup", "dup"))
   evaluate(Right(sta))
   override def evaluate(state: Result): Result =
      if udf.contains(word) then
         println("word found"); execute(udf(word), Nil)
      else Left(ForthError.UnknownWord)

class Forth extends ForthEvaluator:
   def eval(text: String): Result =
      val words = text.toLowerCase.split(" ").toList
      execute(words, Nil)

@tailrec
def execute(ws: List[String], st: List[Int]): Result =
   if ws.isEmpty then Right(State(st))
   else
      (ws, st) match
         case (num :: tail, r) if num.forall(_.isDigit)    => execute(tail, num.toInt :: r)
         case ("+" :: tail, t :: f :: r) if st.size >= 2   => execute(tail, t + f :: r)
         case ("-" :: tail, t :: f :: r) if st.size >= 2   => execute(tail, f - t :: r)
         case ("*" :: tail, t :: f :: r) if st.size >= 2   => execute(tail, f * t :: r)
         case ("/" :: tail, t :: f :: r) if st.size >= 2   =>
            if t == 0 then Left(ForthError.DivisionByZero) else execute(tail, f / t :: r)
         case ("dup" :: tail, t :: r) if st.nonEmpty       => execute(tail, t :: t :: r)
         case ("drop" :: tail, _ :: r) if st.nonEmpty      => execute(tail, r)
         case ("swap" :: tail, t :: f :: r) if st.nonEmpty => execute(tail, f :: t :: r)
         case ("over" :: tail, t :: f :: r) if st.nonEmpty => execute(tail, f :: t :: f :: r)
         case (";" :: tail, r) if st.nonEmpty              => execute(tail, r)
         case (":" :: tail, r)                             =>
            val newWord = tail.takeWhile(_ != ";").head
            if newWord.forall(_.isDigit) then Left(ForthError.InvalidWord)
            else
               val newDefinition = tail.takeWhile(_ != ";").tail
               val remainder = tail.dropWhile(_ != ";").tail
               // println((remainder, java.time.LocalTime.now()))
               Def(newWord, newDefinition, State(st))
               execute(remainder, r)

         case (_, r) if r.size == 1 | r == Nil => Left(ForthError.StackUnderflow)
         case _                                => Left(ForthError.UnknownWord)
