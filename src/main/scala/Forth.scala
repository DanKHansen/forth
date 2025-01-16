import ForthError.ForthError

class Forth extends ForthEvaluator:
   def eval(text: String): Either[ForthError, ForthEvaluatorState] =
      val words = text.split(" ").toList
      parse(words)

   private def parse(strings: List[String]): Either[ForthError, ForthEvaluatorState] =
      if strings.isEmpty then Left(ForthError.StackUnderflow)
      else
         val numbers: List[Int] = strings.takeWhile(_.matches("\\d")).map(_.toInt)
         val st = numbers
         Right(
           new ForthEvaluatorState:
              override val state: List[Int] = st
         )
