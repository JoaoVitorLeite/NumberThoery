package numtheory

import Num.Implicits._

case class ChineseAux[A](number: A, moduli: A)(implicit op: Num[A]){ }
