import ForthError.ForthError

import scala.annotation.tailrec
import scala.collection.immutable.::

type Word = String
type Words = List[Word]
type Result = Either[ForthError, ForthEvaluatorState]

class State(val stack: List[Int]) extends ForthEvaluatorState:
   override def toString: String = super.toString

class Forth extends ForthEvaluator:
   private val dictionary: List[(Word, Words)] = List(
     "+" -> List("+"),
     "-" -> List("-"),
     "*" -> List("*"),
     "/" -> List("/"),
     "dup" -> List("dup"),
     "drop" -> List("drop"),
     "swap" -> List("swap"),
     "over" -> List("over"),
     "dup-twice" -> List("dup", "dup")
   )
   def eval(text: String): Either[ForthError, ForthEvaluatorState] =
      @tailrec
      def interpret(words: Words, stack: List[Int] = Nil): Result =
         if words.isEmpty then Right(State(stack))
         else
            (words, stack) match
               case (w :: ws, st) =>
                  w match
                     case num if num.forall(_.isDigit) => interpret(ws, addToStack(num, st))
                     case _                            =>
                        val defn = dictionary.groupBy(_._1)(w).head._2
                        execute(defn.head, st) match
                           case Left(e)      => Left(e)
                           case Right(newSt) => interpret(ws, newSt)
               case _             => Left(ForthError.UnknownWord)

      interpret(text.toLowerCase.split(" ").toList)

   private def addToStack(w: Word, s: List[Int] = Nil) = w.toInt :: s

   private def execute(w: Word, s: List[Int]): Either[ForthError, List[Int]] =
      (w, s) match
         case ("+", t :: l :: r)    => Right(t + l :: r)
         case ("-", t :: l :: r)    => Right(l - t :: r)
         case ("*", t :: l :: r)    => Right(l * t :: r)
         case ("/", t :: l :: r)    =>
            if t == 0 then Left(ForthError.DivisionByZero)
            else Right(l / t :: r)
         case ("dup", t :: r)       => Right(t :: t :: r)
         case ("drop", _ :: r)      => Right(r)
         case ("swap", t :: l :: r) => Right(l :: t :: r)
         case ("over", t :: l :: r) => Right(l :: t :: l :: r)
         case _                     => Left(ForthError.InvalidWord)
