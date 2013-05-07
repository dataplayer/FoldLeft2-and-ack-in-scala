object FoldLeftAndAck {

/*
 * A special fold left function
 */

def foldLeft2[A,B](z: A)(xs: List[B],ys: List[B],f: (A,B,B) => A): A = {
  def _foldLeft2(acc: A, xs: List[B], ys: List[B]): A = (xs,ys) match {
		case (Nil,Nil) => acc
		case (_,Nil) => acc
		case (Nil,_) => acc
		case (hxs::txs,hys::tys) => _foldLeft2(f(acc,hxs,hys),txs,tys)
	} 
    _foldLeft2(z,xs,ys)
}

def max(x: Int, y: Int): Int = if (x < y) y else x
def accumulate_max(acc: Int,x: Int,y: Int): Int = acc + max(x,y)


/*
 * Ackermann function in scala
 *
 */
def ack(m: Int, n: Int): Int = {
  require(m >= 0 & n >= 0)
  (m,n) match {
  case (0,_) => n+1
  case (m,0) if (m > 0) => ack(m-1,1)
  case (m,n) if (m > 0 & n > 0) => ack(m-1,ack(m,n-1))
  }
}

//Alternate solution, If case statements are in the right order, you don't need the if statements.
//Slicker, but previous solution would be prefered for quick read/maintenance/clarity.
def ack(m: Int, n: Int): Int = {
  require(m >= 0 & n >= 0)
  (m,n) match {
  case (0,_) => n+1
  case (_,0) => ack(m-1,1)
  case (_,_) => ack(m-1,ack(m,n-1))
  }
}


def main(args: Array[String]) {
  println(foldLeft2(0)(List(1,2,3),List(4,5),accumulate_max))
  println(ack(3,4))
}

}
