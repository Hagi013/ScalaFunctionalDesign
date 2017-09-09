package implicits

trait LabelMaker[T] {
  def toLabel(value: T): String
}

// Adapterクラス
case class AddressLabelMaker() extends LabelMaker[Address] {
  def toLabel(address: Address) = {
    import address._
    "%d %s, %s, %s 〒%s".format(no, street, city, state, zip)
  }
}

object Interface {
  def main(args: Array[String]): Unit = {
    val str = AddressLabelMaker().toLabel(Address(100, "HogeHogeStreet", "Tokyo", "Japan", "101-001"))
    println(str)
  }
}

