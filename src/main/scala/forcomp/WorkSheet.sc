package forcomp

import common._
import Anagrams._

object WorkSheet {

  val w = "Robert"                                //> w  : String = Robert
  (w.toLowerCase() groupBy ((e: Char) => e)).toList.sorted map { case (x, y) => (x, y.length) }
                                                  //> res0: List[(Char, Int)] = List((b,1), (e,1), (o,1), (r,2), (t,1))

  val l = List("The", "Cat", "In", "The", "Hat")  //> l  : List[String] = List(The, Cat, In, The, Hat)
  val s = l.flatten.mkString                      //> s  : String = TheCatInTheHat

  (s.toLowerCase() groupBy ((e: Char) => e)).toList.sorted map { case (x, y) => (x, y.length) }
                                                  //> res1: List[(Char, Int)] = List((a,2), (c,1), (e,2), (h,3), (i,1), (n,1), (t,
                                                  //| 4))
  //dictionary groupBy((w: Word) => wordOccurrences(w))

  val os = List(('a', 2), ('b', 2))               //> os  : List[(Char, Int)] = List((a,2), (b,2))

  val xx = 5                                      //> xx  : Int = 5

  val comb = for {
    i <- 0 to os.head._2
    o <- os take i

  } yield (o._1, i)                               //> comb  : scala.collection.immutable.IndexedSeq[(Char, Int)] = Vector((a,1), (
                                                  //| a,2), (b,2))

  val xs = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
                                                  //> xs  : List[(Char, Int)] = List((a,1), (d,1), (l,1), (r,1))
  val ys = List(('l',1),('r', 1))                 //> ys  : List[(Char, Int)] = List((l,1), (r,1))

  val m = xs.toMap                                //> m  : scala.collection.immutable.Map[Char,Int] = Map(a -> 1, d -> 1, l -> 1, 
                                                  //| r -> 1)
  val n = (ys foldLeft m)((x, y) => {
    println("x: " + x +", y: " + y)
    lazy val d = (x apply y._1) - y._2
    println("d: " + d)
    if (d > 0) x updated (y._1, d)
    else (x - y._1)
  })                                              //> x: Map(a -> 1, d -> 1, l -> 1, r -> 1), y: (l,1)
                                                  //| d: 0
                                                  //| x: Map(a -> 1, d -> 1, r -> 1), y: (r,1)
                                                  //| d: 0
                                                  //| n  : scala.collection.immutable.Map[Char,Int] = Map(a -> 1, d -> 1)
  val sentence = List("Linux", "rulez")           //> sentence  : List[String] = List(Linux, rulez)
  val occs = sentenceOccurrences(sentence)        //> occs  : forcomp.Anagrams.Occurrences = List((e,1), (i,1), (l,2), (n,1), (r,1
                                                  //| ), (u,2), (x,1), (z,1))
  val combos = combinations(occs)                 //> combos  : List[forcomp.Anagrams.Occurrences] = List(List((e,1), (i,1), (l,1)
                                                  //| , (n,1), (r,1), (u,1), (x,1), (z,1)), List((e,1), (i,1), (l,2), (n,1), (r,1)
                                                  //| , (u,1), (x,1), (z,1)), List((e,1), (i,1), (l,1), (n,1), (r,1), (u,2), (x,1)
                                                  //| , (z,1)), List((e,1), (i,1), (l,2), (n,1), (r,1), (u,2), (x,1), (z,1)), List
                                                  //| ((e,1), (i,1), (l,1), (n,1), (r,1), (u,1), (x,1)), List((e,1), (i,1), (l,2),
                                                  //|  (n,1), (r,1), (u,1), (x,1)), List((e,1), (i,1), (l,1), (n,1), (r,1), (u,2),
                                                  //|  (x,1)), List((e,1), (i,1), (l,2), (n,1), (r,1), (u,2), (x,1)), List((e,1), 
                                                  //| (i,1), (l,1), (n,1), (r,1), (u,1), (z,1)), List((e,1), (i,1), (l,2), (n,1), 
                                                  //| (r,1), (u,1), (z,1)), List((e,1), (i,1), (l,1), (n,1), (r,1), (u,2), (z,1)),
                                                  //|  List((e,1), (i,1), (l,2), (n,1), (r,1), (u,2), (z,1)), List((e,1), (i,1), (
                                                  //| l,1), (n,1), (r,1), (u,1)), List((e,1), (i,1), (l,2), (n,1), (r,1), (u,1)), 
                                                  //| List((e,1), (i,1), (l,1), (n,1), (r,1), (u,2)), List((e,1), (i,1), (l,2), (n
                                                  //| ,1), (r,1), (u,2)), List
                                                  //| Output exceeds cutoff limit.
  val combo_0 = combos(0)                         //> combo_0  : forcomp.Anagrams.Occurrences = List((e,1), (i,1), (l,1), (n,1), 
                                                  //| (r,1), (u,1), (x,1), (z,1))
  val rest = subtract(occs, combo_0)              //> rest  : forcomp.Anagrams.Occurrences = List((l,1), (u,1))
  
  //combos.size

  //val word = List(('a',1))
  //val word = combos(1)
  def occAnagrams(occs: Occurrences): List[Sentence] = {
    for {
      c <- combinations(occs)
      if dictionaryByOccurrences.contains(c)
      w <- dictionaryByOccurrences(c)
      rest <- occAnagrams(subtract(occs, c))
    } yield w :: rest
  }                                               //> occAnagrams: (occs: forcomp.Anagrams.Occurrences)List[forcomp.Anagrams.Sent
                                                  //| ence]
  occAnagrams(sentenceOccurrences(sentence))      //> res2: List[forcomp.Anagrams.Sentence] = List()
  //dictionaryByOccurrences.get(word)
  //dictionary contains ("Linux")
}