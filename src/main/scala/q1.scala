object InventoryManagement {

  type Product = (String, Int, Double)
  type Inventory = Map[Int, Product]

  val inventory1: Inventory = Map(
    101 -> ("ProductA", 10, 15.5),
    102 -> ("ProductB", 5, 25.0),
    103 -> ("ProductC", 20, 7.75)
  )

  val inventory2: Inventory = Map(
    101 -> ("ProductA", 5, 18.0),
    104 -> ("ProductD", 12, 12.0),
    105 -> ("ProductE", 7, 20.0)
  )


  def getProductNames: Iterable[String] = inventory1.values.map(_._1);

  def totalPrice: Double = {
    inventory1.values.map(_._3).sum;
  }


  def mergeInventories(): Inventory = {
    var mergedInventory = inventory1

    for ((id, (name, qty, price)) <- inventory2) {
      if (mergedInventory.contains(id)) {
        val (existingName, existingQty, existingPrice) = mergedInventory(id)
        val newQty = existingQty + qty
        val newPrice = existingPrice.max(price)
        mergedInventory = mergedInventory.updated(id, (name, newQty, newPrice))
      } else {
        mergedInventory = mergedInventory + (id -> (name, qty, price))
      }
    }

    mergedInventory
  }

  def availability(productIDToCheck: Int): Unit = {
    inventory1.get(productIDToCheck) match {
      case Some(product) =>
        println(s"Product ID $productIDToCheck exists in Inventory-1: $product")
      case None =>
        println(s"Product ID $productIDToCheck does not exist in Inventory-1")
    }

    inventory2.get(productIDToCheck) match {
      case Some(product) =>
        println(s"Product ID $productIDToCheck exists in Inventory-2: $product")
      case None =>
        println(s"Product ID $productIDToCheck does not exist in Inventory-2")
    }
  }

}

object Main extends App{
  println("Products in Inventory-1:" + InventoryManagement.getProductNames)
  println()

  println("Total value of products in Inventory-1:" + InventoryManagement.totalPrice)
  println()

  println("Is Inventory 1 Empty? " + InventoryManagement.inventory1.isEmpty)
  println()

  val mergedInventory = InventoryManagement.mergeInventories()
  println("Inventory after merging: ")
  mergedInventory.foreach { case (id, (name, qty, price)) =>
    println(s"ID: $id, Name: $name, Quantity: $qty, Price: $price")
  }
  println()

  InventoryManagement.availability(105)
}