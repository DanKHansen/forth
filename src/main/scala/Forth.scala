import ForthError.{DivisionByZero, ForthError, InvalidWord, StackUnderflow, UnknownWord}

import scala.annotation.tailrec

// udf word syntax:        : word def ;
val udfws: Map[String, List[String]] = Map.empty
case class State(stack: List[String]) extends ForthEvaluatorState:
   override def toString: String = stack.mkString(" ")

class Forth extends ForthEvaluator:
   def eval(text: String): Either[ForthError, ForthEvaluatorState] =
      val ws = text.toLowerCase.split(" ").toList
      run(ws).map(w => State(w.reverse))

@tailrec
def run(words: List[String], stack: List[String] = Nil): Either[ForthError, List[String]] =
   //println((words, stack))
   if words.isEmpty then Right(stack)
   else
      (words, stack) match
         case (h :: t, st) if h forall (_.isDigit)       => run(t, h :: st)
         case ("+" :: t, a :: b :: st)                   => run(t, (a.toInt + b.toInt).toString :: st)
         case ("-" :: t, a :: b :: st)                   => run(t, (b.toInt - a.toInt).toString :: st)
         case ("*" :: t, a :: b :: st)                   => run(t, (a.toInt * b.toInt).toString :: st)
         case ("/" :: _, "0" :: _)                       => Left(DivisionByZero)
         case ("/" :: t, a :: b :: st)                   => run(t, (b.toInt / a.toInt).toString :: st)
         case ("dup" :: t, a :: st)                      => run(t, a :: a :: st)
         case ("drop" :: t, _ :: st)                     => run(t, st)
         case ("swap" :: t, a :: b :: st)                => run(t, b :: a :: st)
         case ("over" :: t, a :: b :: st)                => run(t, b :: a :: b :: st)
         case (":" :: w :: _, _) if w forall (_.isDigit) => Left(InvalidWord)

         case (":" :: _, _) =>
            val udf = words.splitAt(words.indexOf(";") + 1)._1.filterNot(s => (s == ":") || (s == ";"))
            val newWords = words.splitAt(words.indexOf(";") + 1)._2
            //println((udf, newWords))
            println(udfws.updated(udf.head, udf.tail))
            run(newWords)
         //case (w :: t, st) => run(t, udfws.getOrElse(w, ))
         case (_, st) if st.isEmpty || st.size <= 1 => Left(StackUnderflow)
         case (_, _)                                => Left(UnknownWord)
