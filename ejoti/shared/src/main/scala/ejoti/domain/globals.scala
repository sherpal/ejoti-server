package ejoti.domain

extension (str: String) {
  def bytes: Array[Byte] = {
    import scala.language.unsafeNulls
    str.getBytes()
  }
}
