package MyParser
import syspro.tm.lexer.Token
import syspro.tm.parser.{AnySyntaxKind, Diagnostic, ParseResult, TextSpan}

import java.util
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*




class CoolParseResult(rootKind: AnySyntaxKind, rootToken: Token) extends ParseResult {
  private val rootNode: CoolSyntaxNode = CoolSyntaxNode(rootKind, rootToken)
  private val invalid_ranges: ListBuffer[TextSpan] = ListBuffer[TextSpan]()
  private val diagnostic: ListBuffer[Diagnostic] = ListBuffer[Diagnostic]()

  override def root(): CoolSyntaxNode = rootNode

  override def invalidRanges(): util.Collection[TextSpan] = invalid_ranges.asJavaCollection

  override def diagnostics(): util.Collection[Diagnostic] = diagnostic.asJavaCollection




}
