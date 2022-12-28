package o1.adventure

import scala.util.Random

/** The plant.
 * @param initialHP  the initial HP of the plant. */

class Plant(val initialHP: Int) {

  private var hp = initialHP
  private var maxhp = initialHP
  private var neededItems = Vector(new Item("water", "The plant looks thirsty, it needs some water.","water"),   // The three different types of items the plant needs
                                   new Item("soil", "The plant's soil looks old and moldy, you might want to replace it.","soil"),
                                   new Item("fertilizer", "The plant looks hungry, it needs some fertilizer.", "fertilizer"))

  var needs = neededItems.head

  def isDead = hp == 0

  def getHP = hp

  def getMaxHP = maxhp

  /** Lower's the plant's HP by 1. This method is called every time the game's clock advances by one hour. */
  def advanceHour() = hp -= 1

  /** Randomly chooses the next type of item needed by the plant. This method is called when the previous need is satisfied. */
  def newNeed() ={
     needs = neededItems(Random.nextInt(3))
     "\n!!! " + needs.description + " !!!"
  }

  /** Uses the given item on the plant. Adds 10 hp to the plant if the given item is the same type that what the plant needs. Returns a description depending on whether the item was accepted or not. */
  def givePlant(item: Item) ={
    if(item.itemType == needs.itemType){
      hp += 10
      if(hp > maxhp) hp = maxhp
      neededItems = neededItems :+ neededItems.head
      neededItems = neededItems.drop(0)
      "This is just what the plant needs! Plant's HP increased by +10." + newNeed()
    }
    else if(item.itemType == "water" || item.itemType == "fertilizer" || item.itemType == "soil"){
      "It doesn't look like the plant needs that right now..."
    }
    else if(item.itemType == "pot"){
      this.maxhp += 20
      "You carefully move the plant into the new pot, it looks much happier now! Plant's max HP increased by +20."
    }
    else "I don't think the plant would want that."
  }

  override def toString = "\n" + this.needs.description + s"\nPlant HP: ${this.hp}/${this.maxhp}"

}
