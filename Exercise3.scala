

object Exercise3 {
  
  def  evenPlaces[A] (alist: List[A]): List[A] =
  {
    if (alist.isEmpty) Nil
    else alist.head :: evenPlaces(skipOdd(alist.tail))
  }
  
  def evenPlaces2[A] (alist: List[A]): List[A] = alist match
  {
    case Nil => Nil
    case n :: rest => n :: evenPlaces2(skipOdd(rest))
    
  }
  
  def skipOdd[A] (alist: List[A]): List[A] =
  {
    if (alist.isEmpty) Nil
    else alist.tail
  }
  
  def  zipMap[A, B] (alist: List[A], funList: List[A => B]): List[B] =
  {
    if (alist.isEmpty || funList.isEmpty) Nil
    else funList.head.apply(alist.head) :: zipMap(alist.tail,funList.tail)
  }
  
  def map2[A,B,C](o1: Option[A], o2: Option[B],f:(A,B)=>C): Option[C] =
  {
    if (o1.isEmpty || o2.isEmpty) None
    else Option(f.apply(o1.get, o2.get))
   }

  
  def main(args: Array[String]): Unit =
  {
    val mylist = List("a", "b", "c", "d")

    println(evenPlaces2(mylist)) // List("a", "c")
    
    //val mylist = List("a", "bb", "2018")
  //  val myfunlist: List[String => Int] = List(s => s.length,   s => s.length*5,  s => s.toInt)
  //  println(zipMap(mylist, myfunlist)) 
    val alist = Pair2(5, Pair2(6, Pair2(7, Pair2(8, Empty2))))
    val alist2 = Pair(5, Pair(6, Pair(7, Pair(8, Empty))))
    println(alist.takeWhile((a:Int) => a <7))
    println(alist2.foldLeft("start", (s:String, i) => s + i.toString))  // "start5678"
    println(alist2.dmap((x => x.toString + x), (y => y.toString+y+y)))
    println(alist2.enumerate)
    val n1 = Plus(Number(3), Mul(Number(4), Number(5)))
    println(n1.eval2)
    println(n1.complexity2)
    val int1 = Some(5)
    val int2 = Some(1)
    println(map2(int1,int2, (a:Int,b:Int)=>a+b))

  }
}
