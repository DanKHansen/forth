import scala.annotation.tailrec
type Result = Either[ForthError.Value, ForthEvaluatorState]

case class State(stack: List[Int]) extends ForthEvaluatorState:
   override def toString: String = super.toString

class Forth extends ForthEvaluator:
   def eval(text: String): Result =
      execute(text.toLowerCase.split(" ").toList, Right(State(Nil)))

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

def udf(cs: List[String], st: Result, dict: Map[String, String]): Result =
   val ws = cs.map(_.toLowerCase).takeWhile(_ != ";").drop(1)
   if ws.head.forall(_.isDigit) then Left(ForthError.InvalidWord)
   else
      val newDict = dict + (ws.head -> ws.tail
         .map(word => dict.getOrElse(word, word))
         .mkString(" "))
      val newWS = cs.diff(ws).drop(2)
      execute(newWS, st, newDict)

@tailrec
def execute(ws: List[String], st: Result, dict: Map[String, String] = Map().empty): Result =
   ws.headOption match
      case Some(num) if num.forall(_.isDigit) => execute(ws.tail, Num(num.toInt).evaluate(st), dict)
      case Some(w)                            =>
         dict.get(w) match
            case Some(s) => execute(s.split(" ").toList ::: ws.tail, st, dict)
            case None    =>
               w match
                  case "+"    => execute(ws.tail, Add.evaluate(st), dict)
                  case "-"    => execute(ws.tail, Sub.evaluate(st), dict)
                  case "*"    => execute(ws.tail, Mul.evaluate(st), dict)
                  case "/"    => execute(ws.tail, Div.evaluate(st), dict)
                  case "dup"  => execute(ws.tail, Dup.evaluate(st), dict)
                  case "drop" => execute(ws.tail, Drop.evaluate(st), dict)
                  case "swap" => execute(ws.tail, Swap.evaluate(st), dict)
                  case "over" => execute(ws.tail, Over.evaluate(st), dict)
                  case ":"    => udf(ws, st, dict)
                  case _      => Left(ForthError.UnknownWord)
      case None                               => st
