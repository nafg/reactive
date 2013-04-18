package reactive
package web
package demo
package snippet

import reactive.web.html._

import net.liftweb.util.Helpers
import Helpers._

import scala.xml.{NodeSeq,Text}

class SelectDemo extends PageSnippet {

  val OSvariants = Map(
    "Windows" -> List("Windows XP", "Windows Vista", "Windows 7"),
    "Linux" -> List("Ubuntu", "Kubuntu", "Fedora")
  )
  val vowels: Set[Char] = "AEIOU".toSet
  def consonantsInName(s: String) = s.toUpperCase.filter(c => c.isLetter && !vowels.contains(c)).toList.distinct
  def vowelsInName(s: String) = s.toUpperCase.filter(vowels).toList.distinct

  val OSSelect = Select(Val(OSvariants.keys.toList))

  val variantSelect = Select(OSSelect.selectedItem.map(_.toList.flatMap(OSvariants)))

  val consonantSelect = Select(variantSelect.selectedItem.map(_.getOrElse("---")).map(consonantsInName))
  val vowelSelect = Select(variantSelect.selectedItem.map(_.getOrElse("---")).map(vowelsInName))

  def render =
    "#os" #> OSSelect &
      "#variant" #> variantSelect &
      "#consonant" #> consonantSelect &
      "#vowel" #> vowelSelect &
      "#feedback" #> Cell {
        for {
          c <- consonantSelect.selectedItem
          v <- vowelSelect.selectedItem
        } yield { _: NodeSeq => Text("You selected "+(c.toList ::: v.toList).mkString(" and ")) }
      }
}
