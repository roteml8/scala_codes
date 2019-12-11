

sealed trait MyList[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: MyList[A]
  def foldLeft[B](initial: B, f: (B, A) => B): B =
  {
    def loop(i: B, l: MyList[A]): B =
    {
      if (l.isEmpty) i
      else loop(f.apply(i, l.head), l.tail)
    }
    loop(initial,this)
  }
  def takeWhile(p: A => Boolean): MyList[A] =
  {
    if (isEmpty) Empty
    else if (p.apply(head)) Pair(head,tail.takeWhile(p))
    else Empty
  }
  def enumerate: MyList[(Int, A)] =
  {
    def getIndex(i:Int, list: MyList[A]): MyList[(Int,A)] =
    {
      if (list.isEmpty) Empty
      else Pair((i,list.head),getIndex(i+1,list.tail))
    }
    getIndex(0,this)
  }
  def dmap[B](f: A => B, g: A => B): MyList[B] = {
    
    def evenOrOdd(i: Int, f:A=>B, g:A=>B, l:MyList[A]): MyList[B] =
    {
      if (l.isEmpty) Empty
      else if (i % 2 == 0) Pair(f.apply(l.head),evenOrOdd(i+1,f,g,l.tail))
      else Pair(g.apply(l.head),evenOrOdd(i+1,f,g,l.tail))
    }
    evenOrOdd(0,f,g,this)

  }
}

final case class Pair[A](head: A, tail: MyList[A]) 
     extends MyList[A] {  
  def isEmpty = false
}

final case object Empty extends MyList[Nothing] {
  def head: Nothing = 
     throw new NoSuchElementException("head of empty list")

  def tail: MyList[Nothing] = 
     throw new NoSuchElementException("tail of empty list")
 
  def isEmpty = true
} 


