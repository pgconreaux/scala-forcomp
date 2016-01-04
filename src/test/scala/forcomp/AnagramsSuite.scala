package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("wordOccurrences: AarDvArK") {
    assert(wordOccurrences("AarDvArK") === List(('a', 3), ('d', 1), ('k', 1), ('r', 2), ('v', 1)))
  }

  test("wordOccurrences: I") {
    assert(wordOccurrences("I") === List(('i', 1)))
  }

  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: You olive") {
    assert(sentenceOccurrences(List("You", "olive")) === List(('e', 1), ('i', 1), ('l', 1), ('o', 2), ('u', 1), ('v', 1), ('y', 1)))
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }

  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("word anagrams empty for no-match") {
    assert(wordAnagrams("hecubus") === Nil)
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: lard - l") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val l = List(('l', 1))
    val rad = List(('a', 1), ('d', 1), ('r', 1))
    assert(subtract(lard, l) === rad)
  }

  test("subtract: lard - lrd") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val lrd = List(('d', 1), ('l', 1), ('r', 1))
    val a = List(('a', 1))
    assert(subtract(lard, lrd) === a)
  }

  test("subtract: dollar - l") {
    val dollar = List(('a', 1), ('d', 1), ('l', 2), ('o', 1), ('r', 1))
    val l = List(('l', 1))
    val dolar = List(('a', 1), ('d', 1), ('l', 1), ('o', 1), ('r', 1))
    assert(subtract(dollar, l) === dolar)
  }

  test("subtract:  - d") {
    val dollar = List(('a', 1), ('d', 1), ('l', 2), ('o', 1), ('r', 1))
    val d = List(('d', 1))
    val ollar = List(('a', 1), ('l', 2), ('o', 1), ('r', 1))
    assert(subtract(dollar, d) === ollar)
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2)))
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez"))
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

}
