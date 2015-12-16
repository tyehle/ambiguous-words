import scala.collection.mutable
import scala.io.Source
import scala.util.Random

/**
 * @author Tobin Yehle
 */
object Replacer {
  def main(args: Array[String]) {
    val duplicates = getGroups(Source.fromFile("100000.txt").getLines().toSeq)
    println(duplicates.size + " sets found")
//    println(duplicates.map(_.mkString(", ")).mkString("\n"))

    val text = processInput(Source.fromFile("the-brave-tin-soldier.txt").mkString, duplicates)

    println(text)
  }

  /**
   * Finds all groups of words that could be reasonably replaced by one another.
   * @param words A number of words to find matches in
   * @return A set of groups of words that look similar
   */
  def getGroups(words: Seq[String]): Seq[Set[String]] = {
    val groups = words.filter(_.length > 0).toSet.groupBy(canonicalize)
    groups.filter(_._2.size > 1).values.toSeq
  }

  /**
   * Splits a word into two vowel/consonant groups on the ends and a middle. A group is a contiguous sequence of either
   * all consonants or all vowels. The middle group contains all the letters not in the other two groups in a canonical
   * order.
   * @param word The word to split
   * @return The word split into three pieces
   */
  def canonicalize(word: String): (String, String, String) = {
    val vowels = Set('a', 'e', 'i', 'o', 'u')

    val firstIsVowel = vowels.contains(word.head)
    val first = word.takeWhile(vowels.contains(_) == firstIsVowel)

    val lastIsVowel = vowels.contains(word.last)
    val last = word.drop(first.length).reverse.takeWhile(vowels.contains(_) == lastIsVowel).reverse

    val middle = word.drop(first.length).dropRight(last.length).sorted

    (first, middle, last)
  }

  /**
   * Replaces some words in an input string. The set of allowed replacements is defined by the groups parameter.
   * @param input A string to replace some of the words of. Yes, I ended a sentence with a preposition. Get over it.
   * @param groups A set of groups of words, where each word in a group could be replaced by any other word in the group
   * @param delimiters Any character that should not be considered text in the input
   * @return The transformed input string
   */
  def processInput(input: String, groups: Seq[Set[String]], delimiters:Set[Char]=" \t\n\r.,?!;:\"()-".toSet): String = {
    // Tokenize the input
    val tokens = mutable.Buffer.empty[Either[String, String]]
    val wordBuffer = mutable.Buffer.empty[Char]
    var inWord = false
    // A simple state machine eats all the characters in the input, adding them to the buffer
    // could be a recursive function, but ... ehh
    for(char <- input) {
      if(wordBuffer.isEmpty) {
        wordBuffer += char
        inWord = !delimiters.contains(char)
      }
      else if(inWord && !delimiters.contains(char) || !inWord && delimiters.contains(char)) {
        wordBuffer += char
      }
      else {
        tokens += (if(inWord) Left(wordBuffer.mkString) else Right(wordBuffer.mkString))
        wordBuffer.clear()
        wordBuffer += char
        inWord = !delimiters.contains(char)
      }
    }
    tokens += (if(inWord) Left(wordBuffer.mkString) else Right(wordBuffer.mkString))

    var wordsTransformed = 0 // This is some impure dirtiness. Deal with it.
    // Deal with a single word
    def processWord(word: String): String = {
      // Keep track of what characters were upper case, so we can put them back
      // This is not perfect, but anything better is WAY harder
      val uppers = word.map(_.isUpper)

      groups.find(_.contains(word.toLowerCase)) match {
        case Some(replacements) =>
          wordsTransformed += 1
          val newWord = replacements.toSeq(Random.nextInt(replacements.size))
          println(s"replaced ${word.chars} with $newWord")
          newWord.zip(uppers).map{case (letter, shouldUpper) => if(shouldUpper) letter.toUpper else letter}.mkString
        case None => word
      }
    }

    // Transform the input
    val result = tokens.map {
      case Left(word) => processWord(word).chars
      case Right(chars) => chars
    }.mkString

    println(s"$wordsTransformed replacements done")

    result
  }
}
