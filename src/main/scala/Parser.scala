package lambda

object Parser:

  private enum Token:
    case TIdent(name: String)
    case TLambda
    case TDot
    case TLParen
    case TRParen

  import Token.*

  def parse(input: String): Term =
    val tokens            = tokenize(input)
    val (term, remaining) = parseTerm(tokens)
    if remaining.nonEmpty then throw ParseError(s"Unexpected trailing tokens: ${remaining.mkString(" ")}")
    term

  case class ParseError(message: String) extends Exception(message)

  private def tokenize(input: String): List[Token] =
    def go(chars: List[Char]): List[Token] =
      chars match
        case Nil                         => Nil
        case c :: rest if c.isWhitespace => go(rest)
        case '\\' :: rest                => TLambda :: go(rest)
        case '.' :: rest                 => TDot :: go(rest)
        case '(' :: rest                 => TLParen :: go(rest)
        case ')' :: rest                 => TRParen :: go(rest)
        case c :: _ if isIdentStart(c) =>
          val (word, rest) = chars.span(isIdentPart)
          TIdent(word.mkString) :: go(rest)
        case c :: _ =>
          throw ParseError(s"Unexpected character: '$c'")
    go(input.toList)

  private def isIdentStart(c: Char): Boolean = c.isLetter
  private def isIdentPart(c: Char): Boolean  = c.isLetterOrDigit || c == '_'

  private def parseTerm(tokens: List[Token]): (Term, List[Token]) =
    tokens match
      case TLambda :: _ => parseAbs(tokens)
      case _            => parseApp(tokens)

  private def parseAbs(tokens: List[Token]): (Term, List[Token]) =
    tokens match
      case TLambda :: rest =>
        val (params, afterParams) = collectParams(rest)
        if params.isEmpty then throw ParseError("Expected at least one parameter name after '\\'")
        afterParams match
          case TDot :: afterDot =>
            val (body, remaining) = parseTerm(afterDot)
            // foldRight desugars \x y z. body into Abs("x", Abs("y", Abs("z", body)))
            val term = params.foldRight(body)((p, acc) => Abs(p, acc))
            (term, remaining)
          case _ =>
            throw ParseError(s"Expected '.' after parameter list, got: ${afterParams.headOption}")
      case _ =>
        throw ParseError(s"Expected '\\' to start abstraction, got: ${tokens.headOption}")

  private def collectParams(tokens: List[Token]): (List[String], List[Token]) =
    @annotation.tailrec
    def go(tokens: List[Token], acc: List[String]): (List[String], List[Token]) =
      tokens match
        case TIdent(name) :: rest => go(rest, name :: acc)
        case _                    => (acc.reverse, tokens)
    go(tokens, Nil)

  private def parseApp(tokens: List[Token]): (Term, List[Token]) =
    val (first, rest) = parseAtom(tokens)
    parseAppTail(first, rest)

  // Left-fold consecutive atoms into nested App nodes: f g h → App(App(f,g),h)
  @annotation.tailrec
  private def parseAppTail(acc: Term, tokens: List[Token]): (Term, List[Token]) =
    tokens match
      case (TIdent(_) | TLParen) :: _ =>
        val (next, rest) = parseAtom(tokens)
        parseAppTail(App(acc, next), rest)
      case _ =>
        (acc, tokens)

  private def parseAtom(tokens: List[Token]): (Term, List[Token]) =
    tokens match
      case TIdent(name) :: rest =>
        (Var(name), rest)
      case TLParen :: rest =>
        val (inner, afterInner) = parseTerm(rest)
        afterInner match
          case TRParen :: tail => (inner, tail)
          case _               => throw ParseError(s"Expected closing ')', got: ${afterInner.headOption}")
      case other =>
        throw ParseError(s"Expected a variable or '(', got: ${other.headOption}")
