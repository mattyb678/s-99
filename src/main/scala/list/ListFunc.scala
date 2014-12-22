package list

import scala.annotation.tailrec
import scala.collection.immutable.::

object ListFunc {

  @tailrec
  def last[T](list: List[T]): T = list match {
    case h :: Nil => h
    case h :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }

  @tailrec
  def penultimate[T](list: List[T]): T = list match {
    case h :: _ :: Nil => h
    case _ :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }

  @tailrec
  def nth[T](n: Int, list: List[T]): T = (n, list) match {
    case (0, h :: _) => h
    case (num, _ :: tail) => nth(num - 1, tail)
    case (_, Nil) => throw new NoSuchElementException
  }

  def length[T](list: List[T]): Int = {
    @tailrec
    def lengthRec[`T`](l: List[T], acc: Int): Int = l match {
      case Nil => acc
      case _ :: tail => lengthRec(tail, acc + 1)
    }
    lengthRec(list, 0)
  }

  def reverse[T](list: List[T]): List[T] = {
    @tailrec
    def reverseRec[`T`](l: List[T], revList: List[T]): List[T] = l match {
      case Nil => revList
      case h :: tail => reverseRec(tail, h :: revList)
    }
    reverseRec(list, List())
  }

  def isPalindrome[T](list: List[T]): Boolean = list match {
    case Nil => true
    case h :: Nil => true
    case h :: t => h == t.last
  }

  // I really struggled getting this to be tail recursive.
  // Copied this solution, but took out the .reverse at the end
  // https://github.com/larsyencken/code-library/blob/master/scala/99-problems/07-flatten.scala
  def flatten[T](list: List[T]): List[T] = {
    @tailrec
    def flattenRec[`T`](ls: List[T], flatList: List[T]): List[T] = ls match {
      case Nil => flatList
      case head :: tail => head match {
        case first: List[T] => flattenRec(first ::: tail, flatList)
        case _ => flattenRec(tail, flatList :+ head)
      }
    }
    flattenRec(list, Nil)
  }

  def compress[T](list: List[T]): List[T] = {
    @tailrec
    def compressRec[`T`](l: List[T], compList: List[T]): List[T] = l match {
      case h :: (tail@h1 :: _) => if (h == h1) compressRec(tail, compList) else compressRec(tail, compList :+ h)
      case h :: Nil => compList :+ h
      case Nil => compList
    }
    compressRec(list, List())
  }

  def pack[T] (ls: List[T]): List[List[T]] = {
    @tailrec
    def packRec[`T`] (list: List[T], curList: List[List[T]]): List[List[T]] = {
      if (list.isEmpty) { List(list) }
      else {
        val (packed, next) = list span { _ == list.head }
        if (next == Nil) curList ++ List(packed)
        else packRec(next, curList ++ List(packed))
      }
    }
    packRec(ls, List())
  }

  def encode[T] (list: List[T]): List[(Int, T)] = {
    pack(list).filter(elem => elem.nonEmpty).map(elem => (elem.size, elem.head))
  }

  def encodeModified[T] (list: List[T]): List[Any] = {
    pack(list).filter(elem => elem.nonEmpty).map(elem => {
      if (elem.size == 1) elem.head else (elem.size, elem.head)
    })
  }

  def decode[T] (list: List[(Int, T)]): List[T] = {
    list flatMap { elem => List.fill(elem._1)(elem._2) }
  }

  def duplicate[T] (list: List[T]): List[T] = {
    list flatMap { elem => List(elem, elem) }
  }

  def duplicateN[T] (n: Int, list: List[T]): List[T] = {
    list flatMap { elem => List.fill(n)(elem) }
  }
}