package list

object ListFunc {

  def last[T] (list: List[T]): T = list match {
    case h :: Nil   => h
    case h :: tail  => last(tail)
    case _          => throw new NoSuchElementException
  }

  def penultimate[T] (list: List[T]): T = list match {
    case h :: _ :: Nil  => h
    case _ :: tail      => penultimate(tail)
    case _              => throw new NoSuchElementException
  }

  def nth[T] (n: Int, list: List[T]): T = (n, list) match {
    case (0, h :: _   ) => h
    case (num, _ :: tail) => nth(num - 1, tail)
    case (_, Nil      ) => throw new NoSuchElementException
  }

  def length[T] (list: List[T]): Int = list match {
    case Nil       => 0
    case _ :: tail => 1 + length(tail)
  }

  def reverse[T] (list: List[T]): List[T] = list match {
    case Nil => List[T]()
    case h :: tail => reverse(tail) ::: List[T](h)
  }

  def isPalindrome[T] (list: List[T]): Boolean = list match {
    case Nil => true
    case h :: Nil => true
    case h :: t => h == t.last
  }

  def flatten[_] (list: List[_]): List[_] = list match {
    case (head: List[_]) :: Nil   => flatten(head)
    case (head: List[_]) :: tail  => flatten(head) ::: flatten(tail)
    case head :: Nil              => List(head)
    case head :: tail             => head :: flatten(tail)
    case Nil                      => List()
  }

  def compress[T] (list: List[T]): List[T] = list match {
    case h :: (tail @ h1 :: _) => if (h == h1) compress(tail) else h :: compress(tail)
    case h :: Nil          => List(h)
    case Nil               => List()
  }
}
