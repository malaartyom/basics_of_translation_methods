package LexerImplementation.Tokens

enum TokenType{
  case Bad
  case BooleanLiteral
  case Identifier
  case Indent(len: Int, start: Int, end: Int)
  case Dedent(len: Int, start: Int, end: Int)
  case IntegerLiteral
  case HardKeyword
  case SoftKeyword
  case RuneLiteral
  case StringLiteral
  case Symbol
  
}




