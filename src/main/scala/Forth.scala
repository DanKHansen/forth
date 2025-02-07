import ForthError.ForthError
import scala.util.{Failure, Success, Try}

class State extends ForthEvaluatorState:
   var stack: List[Int] = List[Int]()
   private val udfWs: Map[String, String] = Map.empty
   def getInputString(name: String): String = udfWs(name)

   override def toString: String = stack.reverse.mkString(" ")

class Forth extends ForthEvaluator:
   def eval(text: String): Either[ForthError, ForthEvaluatorState] =
      val state = new State
      evaluate(state, text.toLowerCase())

   def evaluate(st: State, input: String): Either[ForthError, State] =
      input
         .split(" ")
         .foldLeft[Either[ForthError, State]](Right(st))((e, str) =>
            e match
               case Right(s) => run(s, str)
               case Left(_)  => e)

   private def run(st: State, word: String): Either[ForthError, State] =
      word match
         case w if w.forall(_.isDigit) =>
            st.stack = w.toInt :: st.stack
            Right(st)
         case "+"                      => arithmetic(st, (a, b) => a + b)
         case "-"                      => arithmetic(st, (a, b) => b - a)
         case "*"                      => arithmetic(st, (a, b) => a * b)
         case "/"                      => arithmetic(st, (a, b) => b / a)
         case "dup" | "drop"           => stackOps(st, word, 1)
         case "swap" | "over"          => stackOps(st, word, 2)
         case _                        => Left(ForthError.UnknownWord)

   private def arithmetic(st: State, f: (Int, Int) => Int): Either[ForthError, State] =
      if st.stack.size < 2 then Left(ForthError.StackUnderflow)
      else
         val res = Try(st.stack.take(2).reduce(f))
         res match
            case Failure(_)     => Left(ForthError.DivisionByZero)
            case Success(value) =>
               st.stack = value :: st.stack.drop(2)
               Right(st)

   private def stackOps(st: State, word: String, minSize: Int): Either[ForthError, State] =
      if st.stack.size < minSize then Left(ForthError.StackUnderflow)
      else
         word match
            case "dup"  =>
               st.stack = st.stack.head :: st.stack
            case "drop" =>
               st.stack = st.stack.tail
            case "swap" =>
               st.stack = st.stack(1) :: st.stack.head :: st.stack.drop(2)
            case "over" =>
               st.stack = st.stack(1) :: st.stack.head :: st.stack(1) :: st.stack.drop(2)
         Right(st)

   def udfWords(st: State, word: String): Either[ForthError, State] =
      evaluate(st, st.getInputString(word))
