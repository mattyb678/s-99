package list

import scala.annotation.tailrec

object ListFunc {

  @tailrec
  def last[T] (list: List[T]): T = list match {
    case h :: Nil   => h
    case h :: tail  => last(tail)
    case _          => throw new NoSuchElementException
  }

  @tailrec
  def penultimate[T] (list: List[T]): T = list match {
    case h :: _ :: Nil  => h
    case _ :: tail      => penultimate(tail)
    case _              => throw new NoSuchElementException
  }

  @tailrec
  def nth[T] (n: Int, list: List[T]): T = (n, list) match {
    case (0, h :: _   ) => h
    case (num, _ :: tail) => nth(num - 1, tail)
    case (_, Nil      ) => throw new NoSuchElementException
  }

  def length[T] (list: List[T]): Int = {
    @tailrec
    def lengthRec[`T`] (l: List[T], acc: Int) : Int = l match {
      case Nil       => acc
      case _ :: tail => lengthRec(tail, acc + 1)
    }
    lengthRec(list, 0)
  }

  def reverse[T] (list: List[T]): List[T] = {
    @tailrec
    def reverseRec[`T`] (l: List[T], revList: List[T]): List[T] = l match {
      case Nil => revList
      case h :: tail => reverseRec(tail, h :: revList)
    }
    reverseRec(list, List())
  }

  def isPalindrome[T] (list: List[T]): Boolean = list match {
    case Nil => true
    case h :: Nil => true
    case h :: t => h == t.last
  }

  def flatten[T] (list: List[T]): List[T] = list match {
    case (head: List[T]) :: Nil   => flatten(head)
    case (head: List[T]) :: tail  => flatten(head) ::: flatten(tail)
    case head :: Nil              => List(head)
    case head :: tail             => head :: flatten(tail)
    case Nil                      => List()
  }


  def compress[T] (list: List[T]): List[T] = {
    @tailrec
    def compressRec[`T`] (l: List[T], compList: List[T]): List[T] = l match {
      case h :: (tail @ h1 :: _) => if (h == h1) compressRec(tail, compList) else compressRec(tail, compList :+ h)
      case h :: Nil          => compList :+ h
      case Nil               => compList
    }
    compressRec(list, List())
  }
}
