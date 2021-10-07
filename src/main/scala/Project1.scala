import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import java.io.PrintWriter
import java.io.File

// Created a class to keep track of the position of the letter in a 5X5 array
case class Cord(x: Int, y: Int)

class PlayFair(val phrase: String = "") {
  // Adds the alphabet to the given phrase so that every phrase given will result in a valid key
  private val augmentedPhrase = phrase + "abcdefghijklmnopqrstuvwxyz"
  // Create the key object that uses a map to hold the letters and their coordinates
  private val key: Map[Char, Cord] = Map()
  // Creates two closure functions that are used later to accomplish things that could not be done in previous languages
  private val charAdder = adderCreator()
  private val spacer = spaceCreator()
  // Creates a function stored in a variable that checks if the key object contains a certain character
  private val keyCheck = (char: Char) => if (key.contains(char)) true else false
  // Creates a function that maps each character to a open spot in the key
  private val mapper = (char: Char) =>
    if (!keyCheck(char) && !char.isWhitespace && char != 'j') charAdder(char.toLower)
  // Calls the mapper function on every single element in the augmented phrase
  augmentedPhrase.foreach(mapper)

  /* This is a closure function that takes in characters and outputs a string that has spaces every 5th character it
   * is used in conjunction with a for each statement to get rid of a for loop in a way that was not possible in
   * previous languages that i have used */
   def spaceCreator(): (Char) => String ={
    var counter = 1
    def spaceAdder(char: Char):String = {
      /* Using if statement as an expression which is not possible in java, while also using the semicolon to put
       * multiple lines of code on the same line */
      if(counter % 5 == 0 && counter != 0) {counter += 1 ; char + " "}
      else{counter += 1 ; char.toString}
    }
    spaceAdder
  }

  /* This is another closure function that adds the characters to the key map also using a foreach statement in order
   * to not use a for loop in a wa that is not possible in previous languages*/
  private def adderCreator(): (Char) => Unit = {
    var x = 0
    var y = 0
    def adder(char: Char): Unit = {
      if (y < 5) {
        if (x < 5) {
          key.addOne(char -> Cord(x, y))
          x += 1
        }
        else {
          y += 1
          x = 0
          key.addOne(char -> Cord(x, y))
          x += 1
        }
      }
    }
    adder
  }

  /* This function takes a string as an argument and filters the message to be only characters stripping out white spaces
   * and punctuation marks. Then the function returns pairs of characters, but if they are the same character it changes
   * the copy to an x */
  def getPairs(message: String): ListBuffer[String] = {
    val pairs = new ListBuffer[String]
    // Filters the message so only letters are left
    val cleanMessage = message.filter(_.isLetter)
    var counter = 0
    while (counter <= cleanMessage.length - 1) {
      if (counter == cleanMessage.length-1 || cleanMessage(counter) == cleanMessage(counter + 1))
        pairs.append(cleanMessage(counter).toLower.toString + "x")
      else {
        pairs.append(cleanMessage(counter).toLower.toString + cleanMessage(counter + 1).toLower.toString)
        counter += 1
      }
      counter += 1
    }
    pairs
  }

  /* This method takes a String that is to be encrypted and returns it encrypted using the playfair cypher */
  def encrypt(message: String): String = {
    val pairs = getPairs(message)
    var encrypted = ""
    var result = ""
    pairs.foreach(encrypted += encryption(_, key, encode = true))
    // Uses the closure function defined earlier to add spaces every 5th character
    encrypted.foreach(result += spacer(_) )
    result
  }

  /* This method takes a String that is encrypted using the playfair cypher and decrypts it returning the input
  *  without any space or punctuation and having several added x's where there were double letters */
  def decrypt(message: String): String = {
    val pairs = getPairs(message)
    var result = ""
    pairs.foreach(result += encryption(_, key, encode = false))
    result
  }

  /* This function is used to both do encryption and decryption and is initialized for either process using the encode
  *  flag. When the flag is true that means that the function is encoding, when encode is false that means the function
  *  is decrypting. The function returns the inputted pair of characters as either encrypted or decrypted */
  private def encryption(pair: String, key: Map[Char, Cord], encode: Boolean): String = {
    val pos1 = if (pair(0) != 'j') key(pair(0)) else key('i')
    val pos2 = if (pair(1) != 'j') key(pair(1)) else key('i')
    val increment: Int = if (encode) 1 else -1
    val startWrap: Int = if (encode) 4 else 0
    val endWrap: Int = if (encode) 0 else 4
    /* This chunk of code uses an else if block as an expression and returns certain values depending on what the if
    *  statements returning which can not be done in other languages*/
    // This condition is for is the letters are in the same row in the matrix
    if (pos1.x == pos2.x) key.find(_._2 ==
      Cord(pos1.x, if (pos1.y == startWrap) endWrap else pos1.y + increment)).get._1.toString +
      key.find(_._2 == Cord(pos2.x, if (pos2.y == startWrap) endWrap else pos2.y + increment)).get._1.toString
    // This condition is for if the letters are in the same column in the matrix
    else if (pos1.y == pos2.y) key.find(_._2 ==
      Cord(if (pos1.x == startWrap) endWrap else pos1.x + increment, pos1.y)).get._1.toString +
      key.find(_._2 == Cord(if (pos2.x == startWrap) endWrap else pos2.x + increment, pos2.y)).get._1.toString
    /* This condition is if the letters are not in the same row or column, it creates a triangle with the coordinates
    *  given and then takes the opposite corner for each letter and returns the corresponding letter */
    else key.find(_._2 == Cord(pos2.x, pos1.y)).get._1.toString +
      key.find(_._2 == Cord(pos1.x, pos2.y)).get._1.toString
  }
}

object Project1 extends App {
  // Creates an object that contains the information to be able to read from the file defined by the path given
  val file = Source.fromFile("C:\\Users\\13194\\IdeaProjects\\Awsome\\src\\main\\scala\\The_Hunger_Games.txt")
  // Creates a string from the contents of the file
  val book = file.mkString.filter(_.isLetter)
  // Closes the file
  file.close()
  // Creates a playfair object with a specific phrase
  val playfair = new PlayFair("How Vexingly Quick Daft Zebras jump")

  /*val encrypted = playfair.encrypt("booooooooooooooooooooookp")
  println(encrypted)
  val decrypted = playfair.decrypt(encrypted)
  println(decrypted)*/

  // Writes a file to the given path
  val pw = new PrintWriter(new File("src/main/scala/encrypted.txt" ))
  pw.write(playfair.encrypt(book))
  pw.close()
}