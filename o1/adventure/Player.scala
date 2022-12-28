package o1.adventure

import scala.collection.mutable.Map
import scala.util.Random


/** A `Player` object represents a player character controlled by the real-life user of the program.
  *
  * A player object's state is mutable: the player's location and possessions can change, for instance.
  *
  * @param startingArea  the initial location of the player */
class Player(startingArea: Area) {

  private var currentLocation = startingArea        // tells the player's current location
  private var quitCommandGiven = false              // one-way flag
  val items = Map[String, Item]()
  private var wallet = 0
  private var timeUntilItem = 0                     // a variable used to calculate how many hours must pass until the player can scavenge the junkyard's mound again
  private var hasSearchedBush = false               // booleans used to keep track of which world objects (that give 1 item per game) have already been searched
  private var hasSearchedPost = false
  private var cashNext = true                       // a boolean used to regulate whether the player will find cash or an item next from the mound
  private val cashToFind = Vector(30, 20, 15, 10, 5, 5, 3, 3, 2, 2, 5, 4, 2)  // cash and items that can be found from the world objects
  private val itemsToFind = Map[String, Item]("scavenged fertilizer" -> new Item("scavenged fertilizer", "An old bag of fertilizer found laying around. Still good to use though.", "fertilizer"),
                                              "scavenged soil" -> new Item("scavenged soil", "Soil found outdoors, the plant couldn't be that picky.", "soil"),
                                              "scavenged water" -> new Item("scavenged water", "A dusty bottle of water, you don't dare to look at the expiration date.", "water"),
                                              "old fertilizer" -> new Item("old fertilizer", "An old bag of fertilizer, retro!", "fertilizer"),
                                              "old soil" -> new Item("old soil", "Almost so old that you might find fossils in it.", "soil"),
                                              "old water" -> new Item("old water", "You wonder how many people have already drank and urinated the same water molecules.", "water"))


  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven


  /** Returns the current location of the player. */
  def location = this.currentLocation


  /** Attempts to move the player in the given location. This is successful if there
    * is a neighbor with the same name and the location is open. Returns
    * a description depending if the player moved or not. */
  def go(destination: String): String = {
    val newLocation = this.location.neighbor(destination).getOrElse(this.location)
    if (newLocation != this.location && !newLocation.isClosed){
      this.currentLocation = newLocation
      if(this.timeUntilItem != 0) timeUntilItem -= 1
      "You go towards the " + destination + "."
    }
    else if(newLocation.isClosed) "The shop is closed. You can see a sign: 'Opening times: 8:00-20:00'."
    else "You can't go to the " + destination + " from here!."
  }


  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() = {
    this.quitCommandGiven = true
    ""
  }

  /** Unlike other action methods, this method returns the used object wrapped in an option. Returns None if the player does not have the item. */
  def use(item: String): Option[Item] = {
    if(this.items.contains(item)) Some(this.items(item))
    else None
  }

  /** Makes the player purchase the given item from the current location's vendor and removes the item's price from the wallet. Returns a description depending on whether the purchase was succesful or not. */
  def buy(item: String): String = {
    if(this.location.vendors.nonEmpty) {
      if(this.items.size == 3){
        "'It looks like your pockets are full.'"
      }
      else{
        val bought = this.location.vendors.head._2.buyItem(item, this.wallet)
        if(bought.isDefined && this.location.vendors.head._2.determinePrice(bought.get) <= this.wallet) {
          this.items += bought.get.name -> bought.get
          this.wallet -= this.location.vendors.head._2.determinePrice(bought.get)
          s"'Thank you for your purchase!'\nYou got the ${bought.get}!"
        }
        else if(bought.isDefined && this.location.vendors.head._2.determinePrice(bought.get) > this.wallet) "'It seems you don't have enough money for that.'"
        else "'I don't have anything like that to sell.'"
      }
    }
    else "There's no one here to buy items from."
  }

  /** Makes the player sell the given item to the current location's vendor and adds half of the item's price from the wallet. Returns a description depending on whether the transaction was succesful or not. */
  def sell(item: String) ={
    if(this.location.vendors.isEmpty) "There is no one here to sell items to."
    else if(this.items.contains(item)){
      val payment = this.location.vendors.head._2.sellItem(this.items(item))
      this.items -= this.items(item).name
      this.wallet += payment
      s"'Hmm.. Sure, I'll buy it for ${payment} bucks.'\nYou got ${payment} bucks!"
    }
    else "You have nothing like that to sell."
  }

  /** Calls the current location's vendor's .talk() -method and returns the returned string. */
  def talk(target: String) ={
    if(this.location.vendors.nonEmpty && (target == "shopkeeper" || target == "pot dealer")){
      this.location.vendors(target).talk()
    }
    else if(this.location.chests.nonEmpty && target == "plant"){
      "They say that talking to plants helps them grow, but this one doesn't seem to care for your rambling."
    }
    else "There is no one here like that to talk to!"
  }

  /** Returns a description of the player's inventory.*/
  def inventory() = {
    if(this.items.isEmpty) "You are empty-handed."
    else{
      "You are carrying:\n" + this.items.keys.mkString("\n")
    }
  }

  /** Returns a description of the player's wallet.*/
  def walletAmount() = s"You currently have $wallet bucks on you."

  /** Makes the player pick up the given item from the chest. Returns a description whether the action was succesful or not. */
  def get(itemName: String) ={
    if(this.location.chests.contains("chest") && this.location.chests("chest").contains(itemName) && this.items.size < 3){
      val newItem = this.location.chests("chest").removeItem(itemName)
      this.items += itemName -> newItem.get
      "You pick up the " + itemName + " from your Chest."
    }
    else if(this.items.size == 3) "Your inventory is full! Go sell or store some items before picking this one up."
    else "There is no " + itemName + " here to pick up."
  }

  /** Makes the player store the given item to the chest. Returns a description whether the action was succesful or not. */
  def store(itemName: String) = {
    if(this.location.chests.contains("chest") && this.has(itemName)){
      this.location.chests("chest").storeItem(this.items(itemName))
      this.items -= itemName
      "You drop the " + itemName + "."
    }
    else if(this.location.chests.isEmpty) "There's no chest here to store the item!"
    else "You don't have that!"
  }

  /** Makes the player examine the target, returning the target's description and possibly adding an item to the player's inventory depending on the target. */
  def examine(target: String) : String= {
    if(this.has(target)) "You look closely at the " + this.items(target) + ".\n" + this.items(target).description
    else if(target == "wallet") s"You currently have $wallet bucks on you."
    else if(target == "inventory") this.inventory()
    else if(this.location.vendors.nonEmpty && (target == "shopkeeper" || target == "pot dealer")) this.location.vendors.head._2.description
    else if(this.location.chests.nonEmpty && target == "chest") this.location.chests.head._2.description
    else if(this.location.chests.nonEmpty && target == "plant") "The plant that causes you agony and suffering. It does liven up the place, though."
    else if(this.location.worldObjects.nonEmpty){
      if(this.location.worldObjects.contains("lamp post") && target == "lamp post"){
        if(this.items.size == 3) "You found something, but your inventory is full! Go sell or store some items before picking this one up."
        else if(itemsToFind.isEmpty) "You find nothing."
        else if(!hasSearchedPost){
          val post = this.location.worldObjects("lamp post")
          val dugItem = itemsToFind.head
          itemsToFind -= dugItem._1
          hasSearchedPost = true
          post.description + s"You found: ${dugItem._1}!"
        }
        else this.location.worldObjects("lamp post").description + "There's nothing here anymore."
      }
      else if(this.location.worldObjects.contains("bush") && target == "bush"){
        if(this.items.size == 3) "You found something, but your inventory is full! Go sell or store some items before picking this one up."
        else if(itemsToFind.isEmpty) "You find nothing."
        else if(!hasSearchedBush){
          val dugItem = itemsToFind.head
          itemsToFind -= dugItem._1
          hasSearchedBush = true
          this.location.worldObjects("bush").description + s"\nYou found: ${dugItem._1}!"
        }
        else this.location.worldObjects("bush").description + "There's nothing here anymore."
      }
      else if(this.location.worldObjects.contains("river") && target == "river"){
        if(this.has("empty bottle")){
          this.items -= "empty bottle"
          this.items += "full bottle" -> new Item("full bottle", "A filled bottle of water you gathered from the local river, your plant will love this!", "water")
          this.location.worldObjects("river").description + "You gather some fresh water into the empty bottle. A filled bottle of water has been added to your inventory!"
        }
        else this.location.worldObjects("river").description + "You might be able to gather some of this water if you had an empty bottle..."
      }
      else if(this.location.worldObjects.contains("mound") && target == "mound"){
        if(timeUntilItem == 0){
          val listToSearch = if(cashNext) cashToFind else itemsToFind
          if(listToSearch == cashToFind || itemsToFind.isEmpty){
            val cashAmount = cashToFind(Random.nextInt(cashToFind.size))
            wallet += cashAmount
            timeUntilItem += 5
            cashNext= false
            this.location.worldObjects("mound").description + s"You found ${cashAmount} bucks!"
          }
          else{
            if(this.items.size == 3) "You found something, but your inventory is full! Go sell or store some items before picking this one up."
            else if(itemsToFind.isEmpty) "You find nothing."
            else{
              val dugItem = itemsToFind.head
              itemsToFind -= dugItem._1
              timeUntilItem += 5
              cashNext = true
              this.location.worldObjects("mound").description + s"You found: ${dugItem._1}!"
            }
          }
        }
        else this.location.worldObjects("mound").description + "You dig for a while but find nothing, probably should come back later."
      }
      else "There is nothing like that here to examine!"
    }
    else "There is nothing like that here to examine!"
  }

  /** Returns true if the player has the given item. */
  def has(itemName: String) = this.items.contains(itemName)


  /** Returns a brief description of the player's state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name


}


