package lambda

/** Interactive REPL for the lambda calculus interpreter.
 *
 *  Usage: sbt run
 *
 *  Commands:
 *    <term>              -- parse and evaluate using current strategy and step limit
 *    :strategy normal    -- switch to normal order (default)
 *    :strategy applicative
 *    :strategy cbv       -- call-by-value
 *    :steps <n>          -- set the step limit (default: 1000)
 *    :free <term>        -- show free variables of a term without evaluating
 *    :parse <term>       -- show the parsed AST without evaluating
 *    :help               -- show this help
 *    :quit               -- exit
 */
object Main:

  def main(args: Array[String]): Unit =
    println("Lambda Calculus Interpreter")
    println("Type :help for available commands, :quit to exit.")
    println()
    repl(Strategy.NormalOrder, maxSteps = 1000)

  // ── REPL loop ─────────────────────────────────────────────────

  @annotation.tailrec
  private def repl(strategy: Strategy, maxSteps: Int): Unit =
    print(s"[${strategyName(strategy)}, steps=$maxSteps] > ")
    val line = scala.io.StdIn.readLine()

    if line == null then
      // EOF (e.g. piped input ended)
      println("\nBye!")
    else
      line.trim match

        case ":quit" | ":q" | ":exit" =>
          println("Bye!")

        case ":help" | ":h" =>
          printHelp()
          repl(strategy, maxSteps)

        case s":strategy $name" =>
          parseStrategy(name.trim) match
            case Some(s) =>
              println(s"Strategy set to: ${strategyName(s)}")
              repl(s, maxSteps)
            case None =>
              println(s"Unknown strategy '$name'. Use: normal, applicative, cbv")
              repl(strategy, maxSteps)

        case s":steps $n" =>
          n.trim.toIntOption match
            case Some(limit) if limit > 0 =>
              println(s"Step limit set to: $limit")
              repl(strategy, limit)
            case _ =>
              println(s"Invalid step limit '$n'. Must be a positive integer.")
              repl(strategy, maxSteps)

        case s":free $input" =>
          runFreeVars(input.trim)
          repl(strategy, maxSteps)

        case s":parse $input" =>
          runParse(input.trim)
          repl(strategy, maxSteps)

        case "" =>
          repl(strategy, maxSteps)

        case input if input.startsWith(":") =>
          println(s"Unknown command '${input.takeWhile(_ != ' ')}'. Type :help for help.")
          repl(strategy, maxSteps)

        case input =>
          runEval(input, strategy, maxSteps)
          repl(strategy, maxSteps)

  // ── Command handlers ──────────────────────────────────────────

  private def runEval(input: String, strategy: Strategy, maxSteps: Int): Unit =
    try
      val term   = Parser.parse(input)
      val result = Reduction.evaluate(term, strategy, maxSteps)
      println(s"  = ${pretty(result.term)}")
      if result.normalFormReached then
        println(s"  (normal form reached in ${result.steps} step${plural(result.steps)})")
      else
        println(s"  (stopped after ${result.steps} step${plural(result.steps)} — step limit reached, not a normal form)")
    catch
      case Parser.ParseError(msg) => println(s"  Parse error: $msg")
      case e: Exception           => println(s"  Error: ${e.getMessage}")

  private def runFreeVars(input: String): Unit =
    try
      val term = Parser.parse(input)
      val fv   = FreeVars.freeVars(term)
      if fv.isEmpty then println("  Free variables: (none)")
      else println(s"  Free variables: ${fv.toSeq.sorted.mkString(", ")}")
    catch
      case Parser.ParseError(msg) => println(s"  Parse error: $msg")
      case e: Exception           => println(s"  Error: ${e.getMessage}")

  private def runParse(input: String): Unit =
    try
      val term = Parser.parse(input)
      println(s"  AST: ${term}")
    catch
      case Parser.ParseError(msg) => println(s"  Parse error: $msg")
      case e: Exception           => println(s"  Error: ${e.getMessage}")

  // ── Pretty printer ────────────────────────────────────────────

  /** Print a Term back as human-readable lambda syntax. */
  def pretty(term: Term): String =
    term match
      case Var(name)       => name
      case Abs(param, body) => s"\\$param. ${pretty(body)}"
      case App(func, arg)  =>
        val lhs = func match
          case Abs(_, _) => s"(${pretty(func)})"
          case _         => pretty(func)
        val rhs = arg match
          case App(_, _) => s"(${pretty(arg)})"
          case Abs(_, _) => s"(${pretty(arg)})"
          case _         => pretty(arg)
        s"$lhs $rhs"

  // ── Helpers ───────────────────────────────────────────────────

  private def strategyName(s: Strategy): String = s match
    case Strategy.NormalOrder      => "normal"
    case Strategy.ApplicativeOrder => "applicative"
    case Strategy.CallByValue      => "cbv"

  private def parseStrategy(name: String): Option[Strategy] = name match
    case "normal"      => Some(Strategy.NormalOrder)
    case "applicative" => Some(Strategy.ApplicativeOrder)
    case "cbv"         => Some(Strategy.CallByValue)
    case _             => None

  private def plural(n: Int): String = if n == 1 then "" else "s"

  private def printHelp(): Unit =
    println("""
  Commands:
    <term>                  Parse and evaluate a lambda term
    :strategy normal        Switch to normal-order reduction (default)
    :strategy applicative   Switch to applicative-order reduction
    :strategy cbv           Switch to call-by-value reduction
    :steps <n>              Set the maximum reduction step limit
    :free <term>            Show free variables of a term
    :parse <term>           Show the parsed AST of a term
    :help                   Show this help message
    :quit                   Exit the interpreter

  Syntax:
    Variables:    x, foo, x1
    Abstraction:  \x. body
    Multi-param:  \x y z. body   (sugar for \x.\y.\z. body)
    Application:  f x y          (left-associative, means (f x) y)
    Grouping:     (M)

  Examples:
    (\x. x) y               -- identity applied to y  =>  y
    (\x y. x) a b           -- K combinator            =>  a
    :strategy cbv
    (\x. x) (\y. y)         -- identity applied to identity
    :free \x. x y           -- shows {y} is free
    :steps 10
    (\x. x x) (\x. x x)    -- Omega: hits step limit
    """.stripMargin)