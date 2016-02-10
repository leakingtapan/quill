package io.getquill.sources.sql.mirror

import io.getquill.naming.{ Literal, NamingStrategy }
import io.getquill.sources.mirror.Row
import io.getquill.sources.sql.SqlSource
import io.getquill.sources.sql.idiom.FallbackDialect
import scala.util.Success
import scala.util.Failure
import io.getquill.sources.mirror.MirrorEncoders
import io.getquill.sources.mirror.MirrorDecoders
import io.getquill.sources.SourceConfig
import io.getquill.SqlMirrorSourceConfig

class SqlMirrorSource[N <: NamingStrategy](config: SqlMirrorSourceConfig[N])
    extends SqlSource[MirrorDialect, N, Row, Row]
    with MirrorEncoders
    with MirrorDecoders {

  override def close = ()

  type QueryResult[T] = QueryMirror[T]
  type ActionResult[T] = ActionMirror
  type BatchedActionResult[T] = BatchActionMirror

  def probe(sql: String) =
    if (sql.contains("Fail"))
      Failure(new IllegalStateException("The sql contains the 'Fail' keyword'"))
    else
      Success(())

  case class ActionMirror(sql: String)

  def execute(sql: String) =
    ActionMirror(sql)

  case class BatchActionMirror(sql: String, bindList: List[Row], indexes: List[List[Int]])

  def execute[T](sql: String, bindParams: T => Row => (Row, List[Int])) =
    (values: List[T]) => {
      val bindings = values.map(bindParams).map(_(Row()))
      BatchActionMirror(sql, bindings.map(_._1), bindings.map(_._2))
    }

  case class QueryMirror[T](sql: String, binds: Row, indexes: List[Int], extractor: Row => T)

  def query[T](sql: String, bind: Row => (Row, List[Int]), extractor: Row => T) = {
    val (row, indexes) = bind(Row())
    QueryMirror(sql, row, indexes, extractor)
  }
}
