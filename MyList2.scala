
sealed trait MyList2[+A] {
  def isEmpty: Boolean 
  def head: A
  def tail: MyList2[A]
  
  def foldLeft[B](initial: B, f: (B, A) => B): B = this match
  {
    case Empty2 => initial
    case Pair2(head,tail) => tail.foldLeft(f.apply(initial, head), f)
    
  }
  
  def takeWhile(p: A => Boolean): MyList2[A] = this match
  {
    case Empty2 => Empty2
    case Pair2(head,tail)  if (p.apply(head)) =>  Pair2(head,tail.takeWhile(p)) 
    case Pair2(head,tail)  if (!p.apply(head)) =>  Empty2
    
  }
}
  
case class Pair2[A](head: A, tail: MyList2[A]) extends MyList2[A] {
  def isEmpty = false
}

case object Empty2 extends MyList2[Nothing] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("head of empty list")
  def tail: MyList2[Nothing] = throw new NoSuchElementException("tail of empty list")
}
