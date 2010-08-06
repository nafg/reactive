package reactive

object test extends Application {
  import TransformedSeq.canBuildFrom
  implicit val o = new Observing {}
  val x = new TransformedSeq[Int] { val observing = o; val underlying = List(1,2,3,4) }
  val y = x.map(_ + 1).takeWhile(_ < 4)
  val z = x.flatMap{i => Seq(i,i*2)}
  println(z)
  
  y.deltas foreach println
  x.deltas.fire(Include(1, 2))
  x.deltas.fire(Include(1, 3))
  
  
  println("Now testing signals")
  
  val b = new BufferSignal[Int] {lazy val observing = new Observing{}}
  val c = b.map{s: TransformedSeq[Int] =>
//    println("will map")
    val ret = s.flatMap(i => Seq(i,i*10)).map(_ * 2).filter(_ % 5 == 0)
//    println("mapped")
    ret
  }
  //b.deltas.debug=true
  b.deltas foreach {m => println("b fired " + m + " and is now " + b.now)}
  println("b is now " + b.now)
  println(System.identityHashCode(b.deltas))
  println("c is now " + c.now)
  println(System.identityHashCode(c.deltas))
  c.deltas foreach {m => println("c fired " + m + " and is now " + c.now)}
  b.now += (10, 11)
  b.now += 15
  b.now -= 11
  b.now += 16
  println("b is now " + b.now)
  println("c is now " + c.now)
  
  println("Now testing diffs")
  def randomSeq: List[Int] = if(math.random>.99) Nil else (math.random*10).toInt :: randomSeq
  val a = randomSeq.toSeq
  val dss = new Var(a) with DiffSeqSignal[Int] {def observing = o}
}
