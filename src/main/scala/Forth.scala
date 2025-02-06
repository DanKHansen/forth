import ForthError.ForthError
import scala.collection.mutable

class Forth extends ForthEvaluator:
   private val stack: mutable.Stack[Int] = mutable.Stack()
   private val dictionary: mutable.Map[String, List[String]] = mutable.Map(
     "+" -> List("add"),
     "-" -> List("subtract"),
     "*" -> List("multiply"),
     "/" -> List("divide"),
     "dup" -> List("dup"),
     "drop" -> List("drop"),
     "swap" -> List("swap"),
     "over" -> List("over")
   )
   def eval(text: String): Either[ForthError, ForthEvaluatorState] = ???

   private def add() = ???
   private def subtract() = ???
   private def multiply() = ???
   private def divide() = ???
   private def dup() = ???
   private def drop() = ???
   private def swap() = ???
   private def over() = ???
