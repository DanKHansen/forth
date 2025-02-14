import ForthError.ForthError

import scala.annotation.tailrec

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
     "over" -> List("over")
   )

   def eval(text: String): Either[ForthError, ForthEvaluatorState] =
      @tailrec
      def interpret(words: Words, stack: List[Int] = Nil, dict: List[(Word, Words)]): Result =
         if words.isEmpty then Right(State(stack))
         else
            val (top, rest) = (words.head, words.tail)
            top match
               case num if num.forall(_.isDigit) => interpret(rest, addToStack(num, stack), dict)
               case ":"                          =>
                  if rest.head.forall(_.isDigit) then Left(ForthError.InvalidWord)
                  else
                     updWordsAndDict(rest, dict) match
                        case (ws, upd) => interpret(ws, stack, upd)
               case word                         =>
                  dict.groupBy(_._1).get(word) match
                     case Some(value) =>
                        execute(value.head._2, stack) match
                           case Left(fe)   => Left(fe)
                           case Right(fst) => interpret(rest, fst.stack, dict)
                     case None        => Left(ForthError.UnknownWord)

      interpret(text.toLowerCase.split(" ").toList, dict = dictionary)

   private def updWordsAndDict(oldWS: List[Word], oldDict: List[(Word, Words)]) =
      val (head, tail) = (oldWS.head, oldWS.tail)
      val newDef = tail.takeWhile(_ != ";")
      (tail.drop(newDef.size + 1), addToUdfDictionary(head, newDef, oldDict))

   private def addToStack(w: Word, s: List[Int] = Nil) = w.toInt :: s

   private def addToUdfDictionary(w: Word, ws: Words, dict: List[(Word, Words)]) = dict.appended(w, ws)

   
   @tailrec
   private def execute(ws: Words, s: List[Int] = Nil): Result =
      val fe = (ForthError.StackUnderflow, ForthError.DivisionByZero, ForthError.UnknownWord)
      if ws.isEmpty then Right(State(s))
      else
         (ws, s) match
            case (num :: tail, _) if num.forall(_.isDigit)    => execute(tail, addToStack(num, s))
            case ("+" :: tail, t :: l :: r) if s.size >= 2    => execute(tail, l + t :: r)
            case ("-" :: tail, t :: l :: r) if s.size >= 2    => execute(tail, l - t :: r)
            case ("*" :: tail, t :: l :: r) if s.size >= 2    => execute(tail, l * t :: r)
            case ("/" :: tail, t :: l :: r) if s.size >= 2    =>
               if t == 0 then Left(fe._2) else execute(tail, l / t :: r)
            case ("dup" :: tail, t :: r) if s.nonEmpty        => execute(tail, t :: t :: r)
            case ("drop" :: tail, _ :: r) if s.nonEmpty       => execute(tail, r)
            case ("swap" :: tail, t :: l :: r) if s.size >= 2 => execute(tail, l :: t :: r)
            case ("over" :: tail, t :: l :: r) if s.size >= 2 => execute(tail, l :: t :: l :: r)
            case (_ :: _, _) if s.isEmpty                     => Left(fe._1) // Stack underflow
            case _                                            => Left(fe._3) // Unknown word
