package zio.prelude

trait Debug[-A] {
  def debug(a: A): Debug.Repr
}

object Debug {
  type Renderer = Repr => String

  val defaultRenderer: Renderer = _ match {
    case Repr.Int(v)        => v.toString
    case Repr.Double(v)     => v.toString
    case Repr.Float(v)      => s"${v}f"
    case Repr.Long(v)       => s"${v}L"
    case Repr.Byte(v)       => v.toString
    case Repr.Char(v)       => v.toString
    case Repr.String(v)     => s""""$v""""
    case Repr.Object(ns, n) => s"${ns.mkString(".")}.$n"
    case Repr.Constructor(ns, n, reprs) =>
      s"${ns.mkString(".")}.$n(${reprs.map(kv => s"${kv._1} -> ${kv._2.render()}").mkString(", ")})"
    case Repr.VConstructor(ns, n, reprs) => s"${ns.mkString(".")}.$n(${reprs.map(_.render()).mkString(", ")})"
  }

  def apply[A](implicit debug: Debug[A]): Debug[A] = debug

  def make[A](f: A => Debug.Repr): Debug[A] = f(_)

  sealed trait Repr { self =>
    def render(renderer: Renderer = defaultRenderer): String = renderer(self)
  }

  object Repr {
    import scala.{ Int => SInt, Double => SDouble, Float => SFloat, Long => SLong, Char => SChar, Byte => SByte }
    import java.lang.{ String => SString }

    final case class Int(value: SInt)       extends Repr
    final case class Double(value: SDouble) extends Repr
    final case class Float(value: SFloat)   extends Repr
    final case class Long(value: SLong)     extends Repr
    final case class Byte(value: SByte)     extends Repr
    final case class Char(value: SChar)     extends Repr
    final case class String(value: SString) extends Repr

    final case class Object(namespace: List[SString], name: SString)                                 extends Repr
    final case class Constructor(namespace: List[SString], name: SString, reprs: Map[SString, Repr]) extends Repr
    object Constructor {
      def apply(namespace: List[SString], name: SString, repr: (SString, Repr), reprs: (SString, Repr)*): Repr =
        new Constructor(namespace, name, (repr :: reprs.toList).toMap)
    }
    final case class VConstructor(namespace: List[SString], name: SString, reprs: List[Repr]) extends Repr
  }

  implicit val NothingDebug: Debug[Nothing] = n => n
  implicit val UnitDebug: Debug[Unit]       = _ => Repr.Object("scala" :: Nil, "()")
  implicit val IntDebug: Debug[Int]         = Repr.Int(_)
  implicit val DoubleDebug: Debug[Double]   = Repr.Double(_)
  implicit val FloatDebug: Debug[Float]     = Repr.Float(_)
  implicit val LongDebug: Debug[Long]       = Repr.Long(_)
  implicit val ByteDebug: Debug[Byte]       = Repr.Byte(_)
  implicit val CharDebug: Debug[Char]       = Repr.Char(_)
  implicit val StringDebug: Debug[String]   = Repr.String(_)

  implicit def EitherDebug[E: Debug, A: Debug]: Debug[Either[E, A]] =
    either =>
      either match {
        case Left(e)  => Repr.VConstructor(List("scala"), "Left", List(e.debug))
        case Right(a) => Repr.VConstructor(List("scala"), "Right", List(a.debug))
      }

  implicit def OptionDebug[A: Debug]: Debug[Option[A]] =
    option =>
      option match {
        case None    => Repr.Object(List("scala"), "None")
        case Some(a) => Repr.VConstructor(List("scala"), "Some", List(a.debug))
      }

  implicit def ListDebug[A: Debug]: Debug[List[A]] =
    list => Repr.VConstructor(List("scala"), "List", list.map(_.debug))

  implicit def VectorDebug[A: Debug]: Debug[Vector[A]] =
    vector => Repr.VConstructor(List("scala"), "Vector", vector.map(_.debug).toList)

  implicit def MapDebug[K: Debug, V: Debug]: Debug[Map[K, V]] =
    map => Repr.VConstructor(List("scala"), "Map", List(map.toList.debug))

  implicit def Tuple2Debug[A: Debug, B: Debug]: Debug[(A, B)] =
    tup2 => Repr.VConstructor(List("scala"), "", List(tup2._1.debug, tup2._2.debug))

  implicit def Tuple3Debug[A: Debug, B: Debug, C: Debug]: Debug[(A, B, C)] =
    tuple => Repr.VConstructor(List("scala"), "Tuple3", List(tuple._1.debug, tuple._2.debug, tuple._3.debug))

  implicit def Tuple4Debug[A: Debug, B: Debug, C: Debug, D: Debug]: Debug[(A, B, C, D)] =
    tuple =>
      Repr.VConstructor(List("scala"), "Tuple4", List(tuple._1.debug, tuple._2.debug, tuple._3.debug, tuple._4.debug))

  implicit def Tuple5Debug[A: Debug, B: Debug, C: Debug, D: Debug, E: Debug]: Debug[(A, B, C, D, E)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple5",
        List(tuple._1.debug, tuple._2.debug, tuple._3.debug, tuple._4.debug, tuple._5.debug)
      )

  implicit def Tuple6Debug[A: Debug, B: Debug, C: Debug, D: Debug, E: Debug, F: Debug]: Debug[(A, B, C, D, E, F)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple6",
        List(tuple._1.debug, tuple._2.debug, tuple._3.debug, tuple._4.debug, tuple._5.debug, tuple._6.debug)
      )

  implicit def Tuple7Debug[A: Debug, B: Debug, C: Debug, D: Debug, E: Debug, F: Debug, G: Debug]
    : Debug[(A, B, C, D, E, F, G)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple7",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug
        )
      )

  implicit def Tuple8Debug[A: Debug, B: Debug, C: Debug, D: Debug, E: Debug, F: Debug, G: Debug, H: Debug]
    : Debug[(A, B, C, D, E, F, G, H)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple8",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug
        )
      )

  implicit def Tuple9Debug[A: Debug, B: Debug, C: Debug, D: Debug, E: Debug, F: Debug, G: Debug, H: Debug, I: Debug]
    : Debug[(A, B, C, D, E, F, G, H, I)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple9",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug
        )
      )

  implicit def Tuple10Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple10",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug
        )
      )

  implicit def Tuple11Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple11",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug
        )
      )

  implicit def Tuple12Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple12",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug
        )
      )

  implicit def Tuple13Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple13",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug
        )
      )

  implicit def Tuple14Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug,
    N: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple14",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug,
          tuple._14.debug
        )
      )

  implicit def Tuple15Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug,
    N: Debug,
    O: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple15",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug,
          tuple._14.debug,
          tuple._15.debug
        )
      )

  implicit def Tuple16Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug,
    N: Debug,
    O: Debug,
    P: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple16",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug,
          tuple._14.debug,
          tuple._15.debug,
          tuple._16.debug
        )
      )

  implicit def Tuple17Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug,
    N: Debug,
    O: Debug,
    P: Debug,
    Q: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple17",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug,
          tuple._14.debug,
          tuple._15.debug,
          tuple._16.debug,
          tuple._17.debug
        )
      )

  implicit def Tuple18Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug,
    N: Debug,
    O: Debug,
    P: Debug,
    Q: Debug,
    R: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple18",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug,
          tuple._14.debug,
          tuple._15.debug,
          tuple._16.debug,
          tuple._17.debug,
          tuple._18.debug
        )
      )

  implicit def Tuple19Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug,
    N: Debug,
    O: Debug,
    P: Debug,
    Q: Debug,
    R: Debug,
    S: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple19",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug,
          tuple._14.debug,
          tuple._15.debug,
          tuple._16.debug,
          tuple._17.debug,
          tuple._18.debug,
          tuple._19.debug
        )
      )

  implicit def Tuple20Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug,
    N: Debug,
    O: Debug,
    P: Debug,
    Q: Debug,
    R: Debug,
    S: Debug,
    T: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple20",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug,
          tuple._14.debug,
          tuple._15.debug,
          tuple._16.debug,
          tuple._17.debug,
          tuple._18.debug,
          tuple._19.debug,
          tuple._20.debug
        )
      )

  implicit def Tuple21Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug,
    N: Debug,
    O: Debug,
    P: Debug,
    Q: Debug,
    R: Debug,
    S: Debug,
    T: Debug,
    U: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple21",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug,
          tuple._14.debug,
          tuple._15.debug,
          tuple._16.debug,
          tuple._17.debug,
          tuple._18.debug,
          tuple._19.debug,
          tuple._20.debug,
          tuple._21.debug
        )
      )

  implicit def Tuple22Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug,
    N: Debug,
    O: Debug,
    P: Debug,
    Q: Debug,
    R: Debug,
    S: Debug,
    T: Debug,
    U: Debug,
    V: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple22",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug,
          tuple._14.debug,
          tuple._15.debug,
          tuple._16.debug,
          tuple._17.debug,
          tuple._18.debug,
          tuple._19.debug,
          tuple._20.debug,
          tuple._21.debug,
          tuple._22.debug
        )
      )
}

trait DebugSyntax {
  implicit class DebugOps[A](self: A) {
    def debug(implicit debug: Debug[A]): Debug.Repr = debug.debug(self)
  }
}
