sealed trait Expr {
    def eval: Int = this match {
      case Plus(left,right) => left.eval+right.eval
      case Minus(left,right) => left.eval-right.eval
      case Mul(left,right) => left.eval*right.eval
      case Number(n) => n
    }
    def complexity: Int = this match {
       case Plus(left,right) => left.complexity+right.complexity+1
       case Minus(left,right) => left.complexity+right.complexity+1
       case Mul(left,right) => left.complexity+right.complexity+2
       case Number(n) => 0
       
    }
    def fold[B] (plus: (B, B) => B,
                 minus: (B, B) => B,
                 mul: (B, B) => B,
                 number: Int => B): B = this match {
       case Plus(op1, op2) => {
         val op1Fold = op1.fold(plus, minus, mul, number)
         val op2Fold = op2.fold(plus, minus, mul, number)
         plus(op1Fold, op2Fold)
        }
       case Minus(op1, op2) => {
        val op1Fold = op1.fold(plus, minus, mul, number)
        val op2Fold = op2.fold(plus, minus, mul, number)
        minus(op1Fold, op2Fold)
       }
       case Mul(op1, op2) => {
         val op1Fold = op1.fold(plus, minus, mul, number)
         val op2Fold = op2.fold(plus, minus, mul, number)
         mul(op1Fold, op2Fold)
       }
       case Number(n) => number(n)
   }
    
   def eval2: Int =  this.fold((x:Int, y:Int) => x+y, (x:Int, y:Int) => x-y, (x:Int, y:Int) => x*y, x=>x)
   def complexity2: Int = this.fold((x:Int, y:Int)=>x+y+1,(x:Int, y:Int) =>x+y+1, (x:Int,y:Int)=>x+y+2, x=>0)
}   



final case class Plus(left: Expr, right: Expr) extends Expr
final case class Minus(left: Expr, right: Expr) extends Expr
final case class Mul(left: Expr, right: Expr) extends Expr
final case class Number(n: Int) extends Expr
