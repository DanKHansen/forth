import ForthError.ForthError

object ForthError extends Enumeration:
   type ForthError = Value
   val DivisionByZero, StackUnderflow, InvalidWord, UnknownWord = Value

trait ForthEvaluatorState:
   // TODO: Implement. return the current stack as Text with the element
   // on top of the stack being the rightmost element in the output."
   val state: List[Int]
   override def toString: String = state.mkString(" ")

abstract class Definition:
   def evaluate(state: Either[ForthError, ForthEvaluatorState]): Either[ForthError, ForthEvaluatorState]

trait ForthEvaluator:
   // TODO: Implement evaluation
   def eval(text: String): Either[ForthError, ForthEvaluatorState]
