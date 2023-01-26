package ejoti.domain

import scala.compiletime.ops.int._
import scala.compiletime._

/** Represents a http status code. */
sealed trait Status[+Code <: Int] {
  def code: Code

  type StatusCode <: Code

  override def toString(): String = s"[$code]"
}

object Status {

  type IsSuccess[Code <: Int] = (Code / 100) < 3

  inline private def status[Code <: Int](using codeValue: ValueOf[Code], ev: (Code < 600) =:= true): Status[Code] =
    new Status[Code]:
      type StatusCode = Code
      override def code: Code = codeValue.value

  def Ok: Status[200]               = status[200]
  def BadRequest: Status[400]       = status[400]
  def NotFound: Status[404]         = status[404]
  def MethodNotAllowed: Status[405] = status[405]
  def Internal: Status[500]         = status[500]

  summon[IsSuccess[200] =:= true]
  summon[IsSuccess[300] =:= false]

}
