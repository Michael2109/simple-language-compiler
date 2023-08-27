package compiler.parser

import fastparse.*
import fastparse.SingleLineWhitespace.*

object LexicalParser {

  def keyword[$: P](s: String): P[Unit] = P(s)

  def comment[$: P] = P("#" ~ CharsWhile(_ != '\n', min = 0))

  def wscomment[$: P] = P((CharsWhileIn(" \n") | comment | "\\\n").rep)

  def nonewlinewscomment[$: P] = P((CharsWhileIn(" ") | comment | "\\\n").rep)

  def identifier[$: P]: P[String] = P((letter | "_") ~ (letter | digit | "_").rep).!.filter(!keywordList.contains(_)).log

  def letter[$: P] = P(lowercase | uppercase)

  def lowercase[$: P] = P(CharIn("a-z"))

  def uppercase[$: P] = P(CharIn("A-Z"))

  def digit[$: P] = P(CharIn("0-9"))

  val keywordList: Set[String] = Set(
    "and", "del", "from", "not", "while",
    "as", "elif", "global", "or", "with",
    "assert", "else", "if", "pass", "yield",
    "break", "except", "import",
    "class", "exec", "in", "raise",
    "continue", "finally", "is", "return",
    "for", "lambda", "try", "mutable",
    "let"
  )

  def stringliteral[$: P]: P[String] = P(stringprefix.? ~ (longstring | shortstring))

  def stringprefix[$: P]: P0 = P(
    "r" | "u" | "ur" | "R" | "U" | "UR" | "Ur" | "uR" | "b" | "B" | "br" | "Br" | "bR" | "BR"
  )

  def shortstring[$: P]: P[String] = P(shortstring0("'") | shortstring0("\""))

  def shortstring0[$: P](delimiter: String) = P(delimiter ~ shortstringitem(delimiter).rep.! ~ delimiter)

  def shortstringitem[$: P](quote: String): P0 = P(shortstringchar(quote) | escapeseq)

  def shortstringchar[$: P](quote: String): P0 = P(CharsWhile(!s"\\\n${quote(0)}".contains(_)))

  def longstring[$: P]: P[String] = P(longstring0("'''") | longstring0("\"\"\""))

  def longstring0[$: P](delimiter: String) = P(delimiter ~ longstringitem(delimiter).rep.! ~ delimiter)

  def longstringitem[$: P](quote: String): P0 = P(longstringchar(quote) | escapeseq | !quote ~ quote.take(1))

  def longstringchar[$: P](quote: String): P0 = P(CharsWhile(!s"\\${quote(0)}".contains(_)))

  def escapeseq[$: P]: P0 = P("\\" ~ AnyChar)

  def longinteger[$: P]: P[BigInt] = P(integer ~ ("l" | "L"))

  def integer[$: P]: P[BigInt] = P(octinteger | hexinteger | bininteger | decimalinteger)

  def decimalinteger[$: P]: P[BigInt] = P(nonzerodigit ~ digit.rep | "0").!.map(scala.BigInt(_))

  def octinteger[$: P]: P[BigInt] = P("0" ~ ("o" | "O") ~ octdigit.rep(1).! | "0" ~ octdigit.rep(1).!).map(scala.BigInt(_, 8))

  def hexinteger[$: P]: P[BigInt] = P("0" ~ ("x" | "X") ~ hexdigit.rep(1).!).map(scala.BigInt(_, 16))

  def bininteger[$: P]: P[BigInt] = P("0" ~ ("b" | "B") ~ bindigit.rep(1).!).map(scala.BigInt(_, 2))

  def nonzerodigit[$: P]: P0 = P(CharIn("1-9"))

  def octdigit[$: P]: P0 = P(CharIn("0-7"))

  def bindigit[$: P]: P0 = P("0" | "1")

  def hexdigit[$: P]: P0 = P(digit | CharIn("a-f", "A-F"))


  def floatnumber[$: P]: P[BigDecimal] = P(pointfloat | exponentfloat)

  def pointfloat[$: P]: P[BigDecimal] = P(intpart.? ~ fraction | intpart ~ ".").!.map(BigDecimal(_))

  def exponentfloat[$: P]: P[BigDecimal] = P((intpart | pointfloat) ~ exponent).!.map(BigDecimal(_))

  def intpart[$: P]: P[BigDecimal] = P(digit.rep(1)).!.map(BigDecimal(_))

  def fraction[$: P]: P0 = P("." ~ digit.rep(1))

  def exponent[$: P]: P0 = P(("e" | "E") ~ ("+" | "-").? ~ digit.rep(1))


  def imagnumber[$: P] = P((floatnumber | intpart) ~ ("j" | "J"))

}