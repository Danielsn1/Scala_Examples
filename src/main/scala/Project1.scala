import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import java.io.PrintWriter
import java.io.File

//Created a class to keep track of the position of the letter in a 5X5 array
case class Cord(x: Int, y: Int)

class PlayFair(val phrase: String = "") {
  //adds the alphabet to the given phrase so that every phrase given will result in a valid key
  private val augmentedPhrase = phrase + "abcdefghijklmnopqrstuvwxyz"
  //create the key object that uses a map to hold the letters and their coordinates
  private val key: Map[Char, Cord] = Map()
  //creates two closure functions that are used later to accomplish things that could not be done in previous languages
  private val charAdder = adderCreator()
  private val spacer = spaceCreator()
  //creates a function stored in a variable that checks if the key object contains a certain character
  private val keyCheck = (char: Char) => if (key.contains(char)) true else false
  private val mapper = (char: Char) =>
    if (!keyCheck(char) && !char.isWhitespace && char != 'j') charAdder(char.toLower)
  augmentedPhrase.foreach(mapper)

  private def spaceCreator(): (Char) => String ={
    var counter = 1
    def spaceAdder(char: Char):String = {
      if(counter % 5 == 0 && counter != 0) {counter += 1 ; char + " "}
      else{counter += 1 ; char.toString}
    }
    spaceAdder
  }

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

  def getPairs(message: String): ListBuffer[String] = {
    val pairs = new ListBuffer[String]
    val cleanMessage = message.filter(_.isLetter)
    var counter = 0
    while (counter < cleanMessage.length - 1) {
      if (cleanMessage(counter) == cleanMessage(counter + 1)) pairs.append(cleanMessage(counter).toLower.toString + "x")
      else {
        pairs.append(cleanMessage(counter).toLower.toString + cleanMessage(counter + 1).toLower.toString)
        counter += 1
      }
      counter += 1
    }
    pairs
  }

  def encrypt(message: String): String = {
    val pairs = getPairs(message)
    var encrypted = ""
    var result = ""
    pairs.foreach(encrypted += encryption(_, key, encode = true))
    encrypted.foreach(result += spacer(_) )
    result
  }

  def decrypt(message: String): String = {
    val pairs = getPairs(message)
    var result = ""
    pairs.foreach(result += encryption(_, key, encode = false))
    result
  }

  private def encryption(pair: String, key: Map[Char, Cord], encode: Boolean): String = {
    val pos1 = if (pair(0) != 'j') key(pair(0)) else key('i')
    val pos2 = if (pair(1) != 'j') key(pair(1)) else key('i')
    val increment: Int = if (encode) 1 else -1
    val startWrap: Int = if (encode) 4 else 0
    val endWrap: Int = if (encode) 0 else 4
    if (pos1.x == pos2.x) key.find(_._2 ==
      Cord(pos1.x, if (pos1.y == startWrap) endWrap else pos1.y + increment)).get._1.toString +
      key.find(_._2 == Cord(pos2.x, if (pos2.y == startWrap) endWrap else pos2.y + increment)).get._1.toString
    else if (pos1.y == pos2.y) key.find(_._2 ==
      Cord(if (pos1.x == startWrap) endWrap else pos1.x + increment, pos1.y)).get._1.toString +
      key.find(_._2 == Cord(if (pos2.x == startWrap) endWrap else pos2.x + increment, pos2.y)).get._1.toString
    else key.find(_._2 == Cord(pos2.x, pos1.y)).get._1.toString +
      key.find(_._2 == Cord(pos1.x, pos2.y)).get._1.toString
  }
}

object Project1 extends App {
  val file = Source.fromFile("C:\\Users\\13194\\IdeaProjects\\Awsome\\src\\main\\scala\\The_Hunger_Games.txt")
  val book = file.mkString.filter(_.isLetter)
  file.close()
  val playfair = new PlayFair("How Vexingly Quick Daft Zebras jump")
  val encrypted = playfair.encrypt("book      that is super cool")
  println(encrypted)
  val decrypted = playfair.decrypt(encrypted)
  println(decrypted)

  val pw = new PrintWriter(new File("src/main/scala/encrypted.txt" ))
  pw.write(playfair.encrypt(book))
  pw.close
}