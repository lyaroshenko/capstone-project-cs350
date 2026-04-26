package lambda

/** Hand-written recursive-descent parser for lambda terms.
 *
 * ─────────────────────────────────────────────────────────────
 * Concrete Syntax
 * ─────────────────────────────────────────────────────────────
 *
 *   term  ::= abs | app
 *   abs   ::= '\' ident+ '.' term      -- multi-param sugar
 *   app   ::= atom { atom }            -- left-associative juxtaposition
 *   atom  ::= ident | '(' term ')'
 *   ident ::= [a-zA-Z][a-zA-Z0-9_]*
 *
 * Key rules:
 *   - Abstraction uses backslash:  \x. body
 *   - Multiple params are sugar:   \x y z. body  ≡  \x.\y.\z. body
 *   - Application is left-assoc:  f g h          ≡  (f g) h
 *   - Abstraction body extends as far right as possible.
 *   - Whitespace (spaces, tabs, newlines) is ignored everywhere.
 *
 * Examples:
 *   \x. x                         -- identity
 *   \x y. x                       -- constant (K combinator)
 *   (\x. x) y                     -- application of identity to y
 *   \f x. f (f x)                 -- Church numeral 2
 *   (\x. x x) (\x. x x)          -- Omega (non-terminating)
 *
 * ─────────────────────────────────────────────────────────────
 */
object Parser:

  // ── Token type ───────────────────────────────────────────────

  private enum Token:
    case TIdent(name: String)
    case TLambda            // '\'
    case TDot               // '.'
    case TLParen            // '('
    case TRParen            // ')'

  import Token.*

  // ── Public API ───────────────────────────────────────────────

  /** Parse a string into a [[Term]].
   *
   *  @throws ParseError if the input is not a valid term.
   */
  def parse(input: String): Term =
    val tokens = tokenize(input)
    val (term, remaining) = parseTerm(tokens)
    if remaining.nonEmpty then
      throw ParseError(s"Unexpected trailing tokens: ${remaining.mkString(" ")}")
    term

  /** Wraps a parse failure with a readable message. */
  case class ParseError(message: String) extends Exception(message)

  // ── Tokenizer ────────────────────────────────────────────────

  private def tokenize(input: String): List[Token] =
    def go(chars: List[Char]): List[Token] =
      chars match
        case Nil                          => Nil
        case c :: rest if c.isWhitespace  => go(rest)
        case '\\' :: rest                 => TLambda :: go(rest)
        case '.'  :: rest                 => TDot    :: go(rest)
        case '('  :: rest                 => TLParen :: go(rest)
        case ')'  :: rest                 => TRParen :: go(rest)
        case c :: _ if isIdentStart(c) =>
          val (word, rest) = chars.span(isIdentPart)
          TIdent(word.mkString) :: go(rest)
        case c :: _ =>
          throw ParseError(s"Unexpected character: '$c'")
    go(input.toList)

  private def isIdentStart(c: Char): Boolean = c.isLetter
  private def isIdentPart(c: Char): Boolean  = c.isLetterOrDigit || c == '_'

  // ── Grammar rules ────────────────────────────────────────────

  /** term  ::= abs | app */
  private def parseTerm(tokens: List[Token]): (Term, List[Token]) =
    tokens match
      case TLambda :: _ => parseAbs(tokens)
      case _            => parseApp(tokens)

  /** abs ::= '\' ident+ '.' term
   *
   *  Multi-parameter sugar is desugared right-to-left:
   *    \x y z. body  →  Abs("x", Abs("y", Abs("z", body)))
   */
  private def parseAbs(tokens: List[Token]): (Term, List[Token]) =
    tokens match
      case TLambda :: rest =>
        // Collect one or more parameter names before the dot
        val (params, afterParams) = collectParams(rest)
        if params.isEmpty then
          throw ParseError("Expected at least one parameter name after '\\'")
        afterParams match
          case TDot :: afterDot =>
            val (body, remaining) = parseTerm(afterDot)
            // Desugar: fold right so \x y. body = Abs(x, Abs(y, body))
            val term = params.foldRight(body)((p, acc) => Abs(p, acc))
            (term, remaining)
          case _ =>
            throw ParseError(
              s"Expected '.' after parameter list, got: ${afterParams.headOption}"
            )
      case _ =>
        throw ParseError(s"Expected '\\' to start abstraction, got: ${tokens.headOption}")

  /** Consume consecutive TIdent tokens as parameter names (stops at TDot or other).
   *  Uses an accumulator so the recursion is tail-recursive.
   */
  private def collectParams(tokens: List[Token]): (List[String], List[Token]) =
    @annotation.tailrec
    def go(tokens: List[Token], acc: List[String]): (List[String], List[Token]) =
      tokens match
        case TIdent(name) :: rest => go(rest, name :: acc)
        case _                    => (acc.reverse, tokens)
    go(tokens, Nil)

  /** app ::= atom { atom }   (left-associative)
   *
   *  We parse one mandatory atom then greedily consume more,
   *  folding left into nested [[App]] nodes.
   */
  private def parseApp(tokens: List[Token]): (Term, List[Token]) =
    val (first, rest) = parseAtom(tokens)
    parseAppTail(first, rest)

  @annotation.tailrec
  private def parseAppTail(acc: Term, tokens: List[Token]): (Term, List[Token]) =
    tokens match
      case (TIdent(_) | TLParen) :: _ =>
        // Next token can start another atom → continue application
        val (next, rest) = parseAtom(tokens)
        parseAppTail(App(acc, next), rest)
      case _ =>
        (acc, tokens)

  /** atom ::= ident | '(' term ')' */
  private def parseAtom(tokens: List[Token]): (Term, List[Token]) =
    tokens match
      case TIdent(name) :: rest =>
        (Var(name), rest)
      case TLParen :: rest =>
        val (inner, afterInner) = parseTerm(rest)
        afterInner match
          case TRParen :: tail => (inner, tail)
          case _               =>
            throw ParseError(
              s"Expected closing ')', got: ${afterInner.headOption}"
            )
      case other =>
        throw ParseError(s"Expected a variable or '(', got: ${other.headOption}")