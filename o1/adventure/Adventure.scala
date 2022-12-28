package o1.adventure

import scala.util.Random


/** The class `Adventure` represents text adventure games. An adventure consists of a player and
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game. This class has been edited to suit the new adventure, Plantcare*/
class Adventure {

  /** The title of the adventure game. */
  val title = "------------------PLANTCARE-------------------"

  /** Declaring the game areas, setting the vendors, the chest and world objects in place. */
  private val home          = new Area("Home", "Your own home! The only furniture here is an old chest and the notorious plant. You see your door, which leads to the street.")
  private val street        = new Area("Street", "You are in the town's central street, you can see your home, park and suburbs")
  private val park          = new Area("Park", "You are in the town's small but idyllic park, you can see the forest and the street.")
  private val suburbs       = new Area("Suburbs", "You are in the town's peaceful suburbs, you can see the junkyard, the shop and the street.")
  private val shop          = new Area("Shop", "You are inside the town's shop, the shopkeeper welcomes you in. You see the shop's door, which leads to the suburbs.")
  private val junkyard      = new Area("Junkyard", "You are in the town's junkyard, you wonder how such a small town can produce so much junk. You see a potential mound for digging. You can also see the suburbs.")
  private val forest        = new Area("Forest", "You are in the large forest, you can see a flowing river. You can also see the park.")
  private val hideout       = new Area("hideout", "") // a location outside of game map where the pot dealer is located during the day

  val dealerLocations = Map(street -> Vector(suburbs, park), park -> Vector(street, forest), suburbs -> Vector(street, junkyard), junkyard -> Vector(suburbs), forest -> Vector(suburbs))

  var dealerLocation = hideout

  home.setNeighbors(Vector("street" -> street))
  street.setNeighbors(Vector("home" -> home, "park" -> park, "suburbs" -> suburbs))
  park.setNeighbors(Vector("forest" -> forest, "street" -> street))
  suburbs.setNeighbors(Vector("shop" -> shop, "junkyard" -> junkyard, "street" -> street))
  shop.setNeighbors(Vector("suburbs" -> suburbs))
  forest.setNeighbors(Vector("park" -> park))
  junkyard.setNeighbors(Vector("suburbs" -> suburbs))

  home.setChest()
  forest.setObject("river", new WorldObject("river", "A small river flowing calmly through the woods.\n"))
  junkyard.setObject("mound", new WorldObject("mound", "You dig through the mound looking for something of value...\n"))
  suburbs.setObject("lamp post", new WorldObject("lamp post", "You look behind the tall lamp post...\n"))
  park.setObject("bush", new WorldObject("bush", "You search through the dense bush...\n"))

  val shopkeeper = new Vendor("shopkeeper", "The shopkeeper behind the counter keeps smiling and looking at you. They seem friendly but mildly irritated of your stare.\n",
                                  Map("rainbow water" -> new Item("rainbow water", "The cheapest bottled water money can buy.", "water"),
                                      "arctic water" -> new Item("arctic water", "Fresh water from the arctic circle, it's so cold it's almost frozen.", "water"),
                                      "luomu water" -> new Item("luomu water", "Water bottled in a spring close to the town, a sustainable choice!", "water"),
                                      "biolan fertilizer" -> new Item("biolan fertilizer", "One shits chicken shit.", "fertilizer"),
                                      "organic fertilizer" -> new Item("organic fertilizer", "So organic it smells awful.", "fertilizer"),
                                      "handmade fertilizer" -> new Item("organic fertilizer", "You don't want to know why it's called 'handmade'.", "fertilizer"),
                                      "familiar soil" -> new Item("familiar soil", "This soil looks familiar, did the shopkeeper dig this up from behind the store?", "soil"),
                                      "rich soil" -> new Item("rich soil", "This soil looks nutrient-rich and healthy for the plant.", "soil"),
                                      "ordinary soil" -> new Item("ordinary soil", "This soil is nothing special, you still paid 10 bucks for it.", "soil"),
                                      "empty bottle" -> new Item("empty bottle", "An empty bottle. Perhaps you could fill it up somewhere?", "bottle")),
                                  Vector("'Welcome! You must be the new resident of the town.'", "'I have everything my customers need for to live! Providing that my customers only eat fertilizer.'",
                                         "'A shady guy has been seen wandering the streets at night.'", "'Remember, that my store closes at 20:00, we open again at 8:00'"))

  val dealer = new Vendor("pot dealer", "The Pot Dealer stands in the shadow anxiously looking around them.\n",
                                  Map("illegal water" -> new Item("illegal water", "This water has been acquired through illegal means, don't let anyone see you holding this.", "water"),
                                      "shady fertilizer" -> new Item("shady fertilizer", "This fertilizer has a shaded look.", "fertilizer"),
                                      "dangerous soil" -> new Item("dangerous soil", "This soil seems dangerous, is there a razor hidden inside?", "soil"),
                                      "flower pot" -> new Item("flower pot", "A brown, ceramic pot that is larger than the one you plant is currently planted in. Surely your plant would like some more room for growing?", "pot")),
                                  Vector("'Hey fresh face. I'm the guy that can deal you weed...removing equipment and some pot...s to plant your flowers in.'", "'You can find me circling around here during night-time.'",
                                         "'I've got loads of grass and weed on my hideout, I should probably get a lawnmower.'", "'Come on, piss off if you're not buying anything!'"))

  shopkeeper.stockUp()
  dealer.stockUp()

  shop.setVendor("shopkeeper", shopkeeper)


  /** The character that the player controls in the game. */
  val player = new Player(home)
  /** The plant to take care for */
  val plant = new Plant(30)

  /** The starting amount of hours. The default setting is 12, so the game starts at 12:00. */
  var hourCount = 12
  /** The maximum number of hours until the game ends, the remaining number of hours is hourLimit - hourCount. */
  var hourLimit = 60

  /** Functions for getting the current time and advancing the time by one hour. */
  private var dayCount = 0

  def currentTime() = {
    val time = hourCount
    if(time >= 24) time - (24 * dayCount)
    else time
  }

  def advanceHour() = {
    hourCount += 1
    if(((hourCount - (dayCount * 24)) % 24 == 0)) dayCount += 1
    plant.advanceHour()
    if(21 < currentTime() || currentTime() < 6) moveDealer()       // moves the dealer at night
    else if(currentTime() == 20) shop.isClosed = true              // closes the shop at 20:00
    else if (currentTime() == 6){                                  // moves the dealer to their hideout at 6:00
      dealerLocation.removeVendor
      dealerLocation = hideout
    }
    else if(currentTime() == 8) shop.isClosed = false              // opens the shop at 8:00
    else if(currentTime() == 21){                                  // moves the dealer to the suburbs at 21:00, where it can roam the world
      suburbs.setVendor("pot dealer", dealer)
      dealerLocation = suburbs
    }

  }

  /** Moves the dealer to a random new location neighboring the current location. */
  def moveDealer() = {
    val potentialLocations = dealerLocations(dealerLocation)
    var newLocation = potentialLocations(Random.nextInt(potentialLocations.size))
    dealerLocation.removeVendor
    dealerLocation = newLocation
    newLocation.setVendor("pot dealer", dealer)
  }



  /** Determines if the adventure is complete, that is, if the time runs out. */
  def isComplete = this.hourCount == this.hourLimit

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */
  def isOver = this.isComplete || this.player.hasQuit || this.plant.isDead

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage ={
    var res = this.title
    res += "\n  ▒▒▒▒▒▒▒▒▒▒▒▒▒▒                              \n▓▓▓▓▓▓▓▓▓▓▒▒▒▒▒▒▒▒▒▒        ▒▒▒▒▒▒▒▒▒▒▒▒▒▒    \n▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▒▒▒▒▒▒  ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒  \n          ▓▓▓▓▓▓▓▓▒▒▒▒▒▒▒▒▒▒▓▓▓▓▓▓▓▓▓▓▒▒▒▒▒▒▒▒\n              ▓▓▓▓▓▓▒▒▒▒▓▓▓▓▓▓    ▓▓▓▓▓▓▓▓▒▒▒▒\n"+
      "                    ▒▒▒▒▓▓            ▓▓▓▓▓▓  \n                    ▒▒▓▓                  ▓▓  \n                    ▒▒▓▓                      \n                    ▒▒▓▓                      \n                    ▒▒▓▓                      \n                    ▒▒▓▓                      \n"+
      "                    ▒▒▓▓                      \n                    ▒▒▓▓                      \n          ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██████            \n          ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓████████            \n          ▓▓▓▓▓▓▓▓▓▓▓▓▓▓██████████            \n            ████████████████████              \n"+
      "            ▓▓▓▓▓▓▓▓▓▓██████████              \n            ▓▓▓▓▓▓▓▓▓▓▓▓████████              \n            ▓▓▓▓▓▓▓▓▓▓▓▓▓▓██████              \n              ▓▓▓▓▓▓▓▓▓▓▓▓████                \n              ▓▓▓▓▓▓▓▓▓▓██████                \n                ██▓▓▓▓██████                  \n"+
      "                ████▓▓██████                  \n                  ████████                    "

    res += "\n(NOTE: THIS GAME WORKS BEST WITH AdventureTextUI. IF USING AdventureGUI, PLAY ON FULL SCREEN TO SEE ALL THE TEXT)"
    res += s"\n\nYou have recently moved to a new town and received a plant as a house-warming gift from your mother.\nUnfortunately, you have no money, tools or even running water to care for the plant with." +
      s"\nPeople still don't understand why plants are the worst gifts for a new home..." +
      s"\n\nYour mother will be coming to visit you in ${this.hourLimit - this.hourCount} hours, can you keep the plant alive until then to prove her that you are a responsible adult?\n"+ "(Write 'help' for more information and the command list.)\n" + this.getInfo
     res
  }

  /** Returns the message called by the command 'help'. */
  def help = {
    var res = "\n\nTHE OBJECTIVE\n" + "Your plant is slowly dying and it needs your help! You can examine the plant to find out if it needs water, fertilizer or soil.\nEach time you move to a new location, the game's time advances by an hour and the plant's HP decreases by one.\n" +
               "Using the needed item on the plant increases its HP by 10. " + "The game ends once the plant dies or the time limit is reached."
    res += "\n\nCOMMANDS\n" + "help               = Prints this info message\nquit               = Quits the game\ngo [location name] = Moves the player to the specified location, if the location is neighboring the current location\nget [item name]    = Inserts the item to the player's inventory, if the player is home and the chest contains the said item." +
               "This command has no effect outside of home.\nstore [item name]  = Stores the item to the player's chest, if the player is home and the inventory contains the said item. This command has no effect outside of home.\nexamine [target]   = Prints out info of the target and possibly giving an item." +
               "The player can examine any item, their wallet, their inventory and the objects declared by the area's description.\nbuy [item name]\t   = Buys the item from the vendor, if the vendor is in the same location and has the said item. Removes an amount of money equal to the item's price from the player's wallet\n" +
               "sell [item name]   = Sells the item to the vendor, if the vendor is in the same location and player has the said item. Adds an amount of money equal to half of the item's price.\ntalk [target]      = The player can talk to the vendors, printing out some (possibly) useful information and the vendor's catalogue\n" +"" +
               "use [item name]    = Uses the item on the plant. This command has no effect outside of home\ninventory          = Prints the player's inventory\nwallet             = Prints the player's wallet"
    res
  }


  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on the current HP of the plant, if the plant died or if the player has quit. */
  def goodbyeMessage = {
    if (this.isComplete){
      var message = "Your mom has arrived!\nAfter exchanging hugs you invite her inside and she notices the plant...\n\nShe says: "
      if(plant.getHP < (plant.initialHP / 3)){
        message += "'Oh, your plant looks terrible! I guess it's like a metaphor for your life right now.'"
      }
      else if(plant.getHP > (2 * plant.initialHP / 3)){
        message += "'I'm impressed, your plant looks great!'"
      }
      else {
        message += "'Hey, your plant is alive, nice.'"
      }
      message
    }
    else if (plant.isDead)
      "Oh no, your plant is dead! Now your house feels even emptier than before.\nGame over!"
    else  // game over due to player quitting
      "Quitter!"
  }

  /** Returns the current time, remaining hours and the plant's HP. This will be called every time the player moves to a new location */
  def getInfo = s"\nTIME: ${this.currentTime()}:00\nYOU HAVE ${this.hourLimit-this.hourCount} HOURS. \nPLANT HP: ${plant.getHP}/${plant.getMaxHP}."


  /** Executes the given in-game command. Returns a textual
    * report of what happened, or an error message if the command was unknown.*/

  def executeCommand(command: String): Option[String]  = {
    val commandText = command.trim.toLowerCase
    val verb        = commandText.takeWhile( _ != ' ' )
    val modifiers   = commandText.drop(verb.length).trim

    verb match {
    case "go"         => {    // advances the time by an hour and calls getInfo(), if the move is successful
                        val oldLocation = player.location
                        val res = Some(player.go(modifiers))
                        if(oldLocation != player.location) {
                          advanceHour()
                          if(player.location.vendors.contains("pot dealer")) Option(res.get + "\nYou spot the pot dealer here." + this.getInfo)
                          else Option(res.get + this.getInfo)
                      }
                        else Option(res.get)
                      }
    case "quit"       => Some(player.quit())
    case "talk"       => Some(player.talk(modifiers))
    case "use"        => {    // checks if the player is home and has the said item, otherwise the plant is not given the item
                        val itemToUse = player.use(modifiers)
                        if(itemToUse.isDefined && player.location.chests.nonEmpty) {
                           if(itemToUse.get.name == "full bottle" && plant.needs.itemType == "water") {
                            player.items -= "full bottle"
                            player.items += "empty bottle" -> new Item("empty bottle", "An empty bottle. Perhaps you could fill it up somewhere?", "bottle")
                           }
                           else if(itemToUse.get.itemType == plant.needs.itemType || itemToUse.get.itemType == "pot") player.items -= itemToUse.get.name
                           Some(plant.givePlant(itemToUse.get))
                          }
                        else if(player.location.chests.isEmpty) Some("You can't use that here!")
                        else Some("You don't have that item!")
                      }
    case "buy"        => Some(player.buy(modifiers))
    case "sell"       => Some(player.sell(modifiers))
    case "store"      => Some(player.store(modifiers))
    case "get"        => Some(player.get(modifiers))
    case "examine"    => {    // checks if the player is home and wants to examine the plant, therefore printing the plant's HP and needed item in addition to its description
                        if(modifiers == "plant" && player.location.chests.nonEmpty) Some(player.examine(modifiers) + plant)
                        else Some(player.examine(modifiers))
                      }
    case "help"       => Some(this.help)
    case "inventory"  => Some(player.inventory())
    case "wallet"     => Some(player.walletAmount())
    /** tän käskyn voi jättää huomiotta, tultiin just ensilumilta */
    case "helikopter" => Some("HELIKOPTER HELIKOPTER PARAKOPFER PARAKOPFER \n" * 10 + "██████████████████████████████████████████████████            \n                        ██                                    \n"+
                              "                      ▒▒▒▒▒▒                                  \n                    ▒▒      ▒▒                          ▓▓▓▓  \n            ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒              ▓▓░░░░▓▓\n"+
                              "          ▓▓▓▓                          ▒▒▒▒▒▒        ▓▓░░░░▓▓\n        ▓▓▒▒▓▓    ▓▓▓▓▓▓    ▓▓▓▓▓▓            ▓▓▓▓▓▓▓▓▓▓▓▓▓▓  \n      ▓▓▒▒▒▒▓▓▒▒▒▒▓▓▒▒▓▓▒▒▒▒▓▓▒▒▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▓▓    \n"+
                              "    ▓▓▓▓▓▓▓▓▓▓▒▒▒▒▓▓▓▓▓▓▒▒▒▒▓▓▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▓▓▓▓      \n  ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▓▓▓▓▓▓          \n▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▓▓                \n"+
                              "▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▓▓                  \n  ▒▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓                    \n            ▓▓                ▓▓                              \n"+
                              "      ▓▓    ▓▓                ▓▓    ▓▓                        \n        ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓                          ")
    case other        => Some("Unknown command: \"" + command + "\".")
    }
  }


}

