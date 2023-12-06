//> using scala 3.3.1

trait Discount {
  def apply(price: Int): Int
}

class Voucher(amount: Int) extends Discount {
  def apply(price: Int): Int = math.max(price - amount, 0)

  override def toString(): String = s"Voucher for $amount"
}

class Sale(percent: Double) extends Discount {
  def apply(price: Int): Int = (price * percent).toInt

  override def toString(): String = s"Sale ${percent * 100}%"
}

case class Item(name: String, price: Int)

def applyDiscounts(
    item: Item,
    quantity: Int,
    discount: Option[Discount]
): (Int, Int) = {
  val price = item.price * quantity
  val discounted = discount.fold(price)(_.apply(price))
  (price, discounted)
}

@main def strategyExample(): Unit = {
  val total = List(
    (Item("glass", 12), 1, None),
    (Item("plate", 10), 5, Some(new Voucher(40))),
    (Item("pot", 20), 2, Some(new Sale(0.10)))
  ).map { case (item, quantity, discount) =>
    val (price, discountedPrice) = applyDiscounts(item, quantity, discount)
    println(
      s"Item: ${item.name}, unit price: ${item.price}, quantity: $quantity, total: $price, discounted: $discountedPrice ($discount)"
    )
    discountedPrice
  }.sum

  println(s"Total: $total")
}
