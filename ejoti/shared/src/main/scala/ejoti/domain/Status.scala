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

  def fromCode(value: Int): Status[Int] = new Status[Int] {
    def code: Int = value
  }

  def Ok: Status[200]                = status
  def TemporaryRedirect: Status[307] = status
  def BadRequest: Status[400]        = status
  def Unauthorized: Status[401]      = status
  def Forbidden: Status[403]         = status
  def NotFound: Status[404]          = status
  def MethodNotAllowed: Status[405]  = status
  def Internal: Status[500]          = status

  summon[IsSuccess[200] =:= true]
  summon[IsSuccess[300] =:= false]

}
