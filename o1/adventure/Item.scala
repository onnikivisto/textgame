package o1.adventure


/** The class `Item` represents items in a text adventure game. Each item has a name
  * and a  *  longer description.
  *
  * N.B. It is assumed, but not enforced by this class, that items have unique names.
  * That is, no two items in a game world have the same name.
  *
  * @param name         the item's name
  * @param description  the item's description
  * @param itemType     the item's type, which is used by the plant to identify whether the used item is the same type than the needed item. */
class Item(val name: String, val description: String, val itemType: String){

  /** Returns a short textual representation of the item (its name, that is). */
  override def toString = this.name


}

/** The class WorldObject refers to the different targets for examining within the given location. These include the river, lamp post, bush and the mound.*/
class WorldObject(name: String, description: String) extends Item(name, description, "object"){

}

/** The class 'Chest' is an Item placed in the player's home. It can store other items and has multiple methods for handling its storage. */
class Chest extends Item("chest", "An old antique chest, where you can store items. Your only possession apart from the high-maintenance plant.\n", "chest"){

  var storage = Map[String,Item]()

  /** Adds an item into the storage */
  def storeItem(item: Item) = this.storage += item.name -> item

  /** Removes an item from the storage */
  def removeItem(itemName: String) ={
    val removed = this.storage.get(itemName)
    if(removed.isDefined) this.storage -= itemName
    removed
  }

  /** Checks if the storage contains said item. */
  def contains(itemName: String) = this.storage.contains(itemName)

  def inventory() = {
    if(this.storage.nonEmpty){
      var print = "Your chest contains the following items:\n"
      for(item <- storage){
        print += item._1 + " "
      }
      print
    }
    else "Your chest is empty right now."
    }

}

/** The class 'Vendor' is an Item (yes, people can be items too), that depicts the shopkeeper and dealer. They have their own Chest for storing their stock and possess methods for selling and buying items.
  * @param items a map containing the items on sale
  * @param sayings a vector containing the different things a vendor can say when talked to */
class Vendor(name: String, description: String, val items: Map[String,Item], val sayings: Vector[String]) extends Item(name, description, "vendor"){

  private var stock = new Chest                                   // the Chest which holds the vendor's items
  private val cheapItems = Vector("water", "fertilizer", "soil")  // item types catecorized into vectors to determine the item's price
  private val normalItems = Vector("bottle")
  private var expensiveItems = Vector("pot")
  private var spokenTo = 0                                        // counts how many times the player has spoken to the vendor

  /** Places the items to the vendor's Chest, this method is called at the start of the game. */
  def stockUp() = items.foreach(item => this.stock.storeItem(item._2))

  /** This method returns the item's price. */
  def determinePrice(item: Item) = {
    if(cheapItems.contains(item.itemType)) 10
    else if(normalItems.contains(item.itemType)) 20
    else if(expensiveItems.contains(item.itemType)) 40
    else -1
  }

  /** Returns the bought item wrapped in an option. None if the vendor doesn't have it.*/
  def buyItem(itemName: String, money: Int) ={
    if(!this.stock.contains(itemName)){
      None
    }
    else if(money < determinePrice(this.stock.storage(itemName))){
      Option(this.stock.storage(itemName))
    }
    else{
      val bought = this.stock.storage(itemName)
      this.stock.removeItem(itemName)
      Option(bought)
    }
  }

  /** Adds the given item to the vendor's stock and returns the amount of money that the player receives from the sale. */
  def sellItem(item: Item) ={
    val sellingPrice = determinePrice(item) / 2
    this.stock.storeItem(item)
    sellingPrice
  }

  /** Returns the description of the vendor's stock. This method is called when the vendor is spoken to */
  def catalogue() = {
    if(this.stock.storage.nonEmpty){
      var print = "'Here's what I have:'\n"
      for(item <- this.stock.storage){
        print += item._1 + s", ${this.determinePrice(item._2)} bucks.\n"
      }
      print
    }
  else "'I don't have anything to sell right now.'"
  }

  /** Returns the string that depicts what the vendor says. */
  def talk() = {
    if(spokenTo == sayings.length){
      spokenTo = 1
    }
    val saying = sayings(spokenTo)
    spokenTo += 1
    saying + "\n" + this.catalogue()
  }
}