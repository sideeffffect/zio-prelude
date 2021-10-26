package zio.prelude
package experimental

trait ExcludedMiddle[A] extends Complement[A] {
  def top: A
}

object ExcludedMiddle {

  /**
   * Summons an implicit `Complement[A]`.
   */
  def apply[A](implicit excludedMiddle: ExcludedMiddle[A]): ExcludedMiddle[A] = excludedMiddle
}
