import ForthError.ForthError
import scala.collection.mutable

object ForthError extends Enumeration:
   type ForthError = Value
   val DivisionByZero, StackUnderflow, InvalidWord, UnknownWord = Value

trait ForthEvaluatorState:
   // TODO: Implement. return the current stack as Text with the element
   override def toString: String

abstract class Definition:
   def evaluate(state: Either[ForthError, ForthEvaluatorState]): Either[ForthError, ForthEvaluatorState]

trait ForthEvaluator:
   // TODO: Implement evaluation
   def eval(text: String): Either[ForthError, ForthEvaluatorState]
