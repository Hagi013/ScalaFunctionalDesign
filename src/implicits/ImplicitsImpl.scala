package implicits

trait LabelMakerImc[T] {
  def toLabel(value : T): String
}

object LabelMakerImc {
  implicit object AddressLabelMakerImc extends LabelMakerImc[Address] {
    def toLabel(address: Address): String = {
      import address._
      "%d %s, %s, %s 〒%s".format(no, street, city, state, zip)
    }
  }
}

object LabelSpecialMakerImc {
  implicit object AddressLabelMakerImc extends LabelMakerImc[Address] {
    def toLabel(address: Address): String = {
      import address._
      "%d %s, %s, %s 〒%s Special!!".format(no, street, city, state, zip)
    }
  }
}


object ImplicitsImpl {
  def main(args: Array[String]): Unit = {
    val str1 = printLabel1(Address(100, "HogeHogeStreet", "Tokyo", "Japan", "101-001"))
    val str2 = printLabel2(Address(200, "HogeHogeStreet", "Tokyo", "Japan", "101-001"))
    println(str1)
    println(str2)

    import LabelSpecialMakerImc._
    val str3 = printLabel2(Address(200, "FugaFugaStreet", "Tokyo", "Japan", "102-201"))
    println(str3)
  }

  def printLabel1[T](t: T)(implicit lm: LabelMakerImc[T]): String = lm.toLabel(t)

  def printLabel2[T: LabelMakerImc](t: T) = implicitly[LabelMakerImc[T]].toLabel(t)
}