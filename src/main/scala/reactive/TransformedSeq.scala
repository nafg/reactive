package reactive

import scala.collection.{Seq,SeqLike,immutable}
import scala.collection.generic._
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.ref.WeakReference

trait TransformedSeq[T]
  extends immutable.Seq[T]
  with GenericTraversableTemplate[T, TransformedSeq]
  with SeqLike[T, TransformedSeq[T]] { outer =>
  protected def uid = getClass.getName + "@" + System.identityHashCode(this)
  //println("instantiated TransformedSeq " + getClass + "@" + System.identityHashCode(this) + ": " + this)
  
  trait Transformed[U] extends TransformedSeq[U] {
    implicit val observing = new Observing {}
    protected def xform(index: Int, elem: T): List[(Int, U)]
    protected[reactive] def xform(m: Message[T,T]): List[Message[U,U]] = m match {
      case Include(loc, elem) => xform(loc, elem) map {
        case (loc, elem) => Include(loc, elem)
      }
      case Update(loc, old, elem) => // --> remove then insert
        xform(loc, old).map{case (loc, old) => Remove(loc, old)} ++
          xform(loc, elem).map{case (loc, elem) => Include(loc, elem)}
      case Remove(loc, elem) => xform(loc, elem) map {
        case (loc, elem) => Remove(loc, elem)
      }
      case Batch(ms @ _*) => List(Batch(ms flatMap xform: _*))
    }
    
    override def baseDeltas = LCS.lcsdiff(outer, underlying, (a:T,b:U) => a==b)
   
  }
  trait IndexTransformed[U] extends Transformed[U] {
    protected var index: ArrayBuffer[Int] = initIndex
    protected def initIndex: ArrayBuffer[Int]
    private def fixIndexes(ms: List[Message[U,U]]): List[Message[U,U]] = {
      def merged(ms: List[Message[U,U]]) = ms flatMap {
        case Batch(ms @ _*) => ms
        case m => List(m)
      }
      def idx(m: Message[U,U]) = m match {
        case Include(i,_) => i
        case Remove(i, _) => i
        case Update(i, _, _) => i
      }
      val mergedAndSorted = merged(ms) sortWith {idx(_) < idx(_)}
      var off = 0
      mergedAndSorted map {
        case Include(i, e) => Include(i - off, e)
        case Remove(i, e) => off += 1; Remove(i - off + 1, e)
        case Update(i, a, b) => Update(i - off, a, b)
      }
    }
    override protected[reactive] def xform(m: Message[T,T]): List[Message[U,U]] = m match {
      case Include(n, _) =>
        val ret = super.xform(m)
        index.insert(n,index(n))
        for(j <- n+1 until index.size)
          index(j) += ret.size
        println(uid + " index after " + m + ": " + index)
        fixIndexes(ret)
      case Remove(n, _) =>
        val ret = super.xform(m)
        println(uid + " index: " + index)
        index.remove(n)
        for(j <- n until index.size)
          index(j) -= ret.size
        println(uid + " index after " + m + ": " + index)
        fixIndexes(ret)
      case Update(n, _, _) =>
        val ret = super.xform(m)
        val removes = ret.count{case Remove(_,_)=>true; case _ => false}
        val inserts = ret.length - removes
        val lengthChange = inserts - removes
        index(n) += lengthChange
        for(j <- n + 1 until index.size)
          index(j) += lengthChange
        /* fixIndexes*/ (ret)
      case b: Batch[T,T] => super.xform(b)
    }
  }
  trait Mapped[U] extends Transformed[U] {
    val mapping: T=>U
    protected def xform(index: Int, elem: T) = List((index, mapping(elem)))
  }
  trait FlatMapped[U] extends IndexTransformed[U] {
    println("Instantiating FlatMapped " + uid)
    def mapping: T => Traversable[U]
    protected def initIndex = {
      val index = new ArrayBuffer[Int] {
        override def toString = outer.toString + " -> " + underlying + " : " + toSeq.zipWithIndex.map{case (a,b)=>b+"->"+a}.mkString("[",",","]")
      }
      //index += 0
      var ptr = 0
      for (i <- 0 until outer.size) {
        //print(i + ": " + outer(i) + " -> ")
        //println(mapping(outer(i)))
        index append ptr
        ptr += mapping(outer(i)).size
        //index += ptr //index(i) + mapping(outer(i)).size
        //println("index: " + index)
      }
      index append ptr
      println(uid + " index: " + index)
      index
    }
    protected def xform(n: Int, elem: T) = {
      println(uid + " in xform, index: " + index)
      val i = index(n)
      mapping(elem).toList.zipWithIndex map {
        case (e, m) => (i + m, e)
      }
    }
  }
  trait Filtered extends IndexTransformed[T] {
    def pred: T => Boolean
    protected def initIndex = {
      val index = new ArrayBuffer[Int]
      index += 0
      for(i <- 0 until outer.size) {
        index += index(i) + (if(pred(outer(i))) 1 else 0)
      }
      index
    }
    protected def xform(n: Int, elem: T) =
      if(pred(elem)) List((index(n), elem)) else Nil
      
    override def baseDeltas = {
      println("Calculating filtered baseDeltas with pred " + pred)
      var off = 0
      outer.zipWithIndex.flatMap {case (e,i) =>
        if(pred(e))
          None
        else {
          off += 1
          Some(Remove(i - off + 1, e))
        }
      }
    }
  }
  trait Sliced extends Transformed[T] {
    val from: Int
    val until: Int
    def xform(index: Int, elem: T) = {
      if(index >= from && index < until)
        List((index - from, elem))
      else
        Nil
    }
    override def baseDeltas =
      (0 until from).toList.map{i => Remove(0, outer(i))} ++
      (until until outer.length).toList.map{i => Remove(until-from, outer(i))}
  }
  trait Appended[U >: T] extends Transformed[U] {
    val rest: Traversable[U]
    protected def xform(index: Int, elem: T): List[(Int, U)] = (index, elem) :: Nil
    override def baseDeltas = rest.toSeq.zipWithIndex.map{case (e,i)=>Include(i+outer.length, e)}
  }
  trait PrefixBased extends Transformed[T] {
    val pred: T => Boolean
    protected var valid = {
      val valid = new ArrayBuffer[Boolean]
      outer foreach {valid += pred(_)}
      valid
    }
    protected def calcLastValid = valid.prefixLength(identity) - 1
    protected var lastValid = calcLastValid
    override protected[reactive] def xform(m: Message[T,T]) = {
      m match {
        case Include(n, elem) =>
          val v = pred(elem)
          valid.insert(n, v)
          if(n <= lastValid) {
            if(!v)
              lastValid = n - 1
            else
              lastValid += 1
          } else if(v && n == lastValid + 1) {
            lastValid = calcLastValid
          }
        case Remove(n, _) =>
          valid.remove(n)
          if(n <= lastValid)
            lastValid -= 1
          else if(n == lastValid + 1) {
            lastValid = calcLastValid
          }
        case Update(n, _, elem) =>
          val (prev, v) = (valid(n), pred(elem))
          valid(n) = v
          if(n <= lastValid && !v)  // true -> false b/c <= lastValid
            lastValid = n - 1
          else if(n == lastValid + 1 && (prev!=v))
            lastValid = calcLastValid
        case _ =>
      }
      super.xform(m)
    }
  }
  trait TakenWhile extends PrefixBased {
    override protected def xform(index: Int, elem: T) =
      if(index <= lastValid) List((index, elem)) else Nil
  }
  trait DroppedWhile extends PrefixBased {
    override protected def xform(index: Int, elem: T) =
      if(index > lastValid) List((index, elem)) else Nil
  }

  
  lazy val deltas: EventStream[Message[T,T]] = new EventStream[Message[T,T]] {}

  implicit def observing: Observing
  def underlying: scala.collection.Seq[T]
  def apply(i: Int): T = underlying.apply(i)
  def length = underlying.length
  def iterator = underlying.iterator
  override def companion = TransformedSeq

  protected def newAppended[U >: T](that: Traversable[U])(result: Seq[U]): TransformedSeq[U] = new Appended[U] {
    val rest = that
    val underlying = result
  }
  protected def newMapped[U](f: T => U)(result: Seq[U]): TransformedSeq[U] = new Mapped[U] {
    val mapping = f
    def underlying = result
  }
  protected def newFlatMapped[U](f: T => Traversable[U])(result: Seq[U]): TransformedSeq[U] = new FlatMapped[U] {
    def mapping = f
    def underlying = result
  }
  protected def newFiltered(p: T => Boolean)(result: Seq[T]): TransformedSeq[T] = new Filtered {
    lazy val pred = p
    val underlying = result
  }
  protected def newSliced(_from: Int, _until: Int)(result: Seq[T]): TransformedSeq[T] = new Sliced {
    val from = _from
    val until = _until
    val underlying = result
  }
  protected def newDroppedWhile(p: T => Boolean)(result: Seq[T]): TransformedSeq[T] = new DroppedWhile {
    lazy val pred = p
    val underlying = result
  }
  protected def newTakenWhile(p: T => Boolean)(result: Seq[T]): TransformedSeq[T] = new TakenWhile {
    lazy val pred = p
    val underlying = result
  }
  
  private def getThat[U, That](result: That)(f: Seq[U]=>TransformedSeq[U]): That = result match {
    case s: Seq[U] => f(s).asInstanceOf[That]
    case other => other
  }
  
  override def ++[U >: T, That](xs: TraversableOnce[U])(implicit bf: CanBuildFrom[TransformedSeq[T], U, That]): That = {
    getThat(super.++(xs))(newAppended(xs.toTraversable))
  }
  override def map[U, That](f: T=>U)(implicit bf: CanBuildFrom[TransformedSeq[T], U, That]): That = {
    //println("in map")
    getThat(super.map(f)(bf))(newMapped(f))
  }
  override def collect[U, That](pf: PartialFunction[T, U])(implicit bf: CanBuildFrom[TransformedSeq[T], U, That]): That =
    filter(pf.isDefinedAt _).map(pf)(bf)
  override def flatMap[U, That](f: T => Traversable[U])(implicit bf: CanBuildFrom[TransformedSeq[T], U, That]): That =
    getThat(super.flatMap(f))(newFlatMapped(f))
  override def filter(p: T => Boolean): TransformedSeq[T] =
    newFiltered(p)(super.filter(p))
  override def withFilter(p: T => Boolean): TransformedSeq[T] =
    filter(p)
  override def partition(p: T => Boolean): (TransformedSeq[T], TransformedSeq[T]) = {
    val (a,b) = super.partition(p)
    (newFiltered(p)(a), newFiltered(!p(_))(b))
  }
  override def init: TransformedSeq[T] =
    newSliced(0,size-1)(super.init)
  override def drop(n: Int): TransformedSeq[T] =
    newSliced(n max 0, Int.MaxValue)(super.drop(n))
  override def take(n: Int): TransformedSeq[T] =
    newSliced(0, n)(super.take(n))
  override def slice(from: Int, until: Int): TransformedSeq[T] =
    newSliced(from, until)(super.slice(from, until))
  override def splitAt(n: Int): (TransformedSeq[T], TransformedSeq[T]) = {
    val (a,b) = super.splitAt(n)
    (newSliced(0,n)(a), newSliced(n max 0, Int.MaxValue)(b))
  }
    
  override def dropWhile(p: T=>Boolean): TransformedSeq[T] =
    newDroppedWhile(p)(super.dropWhile(p))
  override def takeWhile(p: T=>Boolean): TransformedSeq[T] =
    newTakenWhile(p)(super.takeWhile(p))
  override def span(p: T=>Boolean): (TransformedSeq[T], TransformedSeq[T]) = {
    val (a, b) = super.span(p)
    (newTakenWhile(p)(a), newDroppedWhile(p)(b))
  }
  
  /**
    The delta from the parent TransformedSeq to this one
  */
  def baseDeltas: Seq[Message[_,T]] = Nil
  
  override protected[this] def newBuilder: Builder[T, TransformedSeq[T]] = new TransformedSeq.TransformedBuilder(observing)
}
object TransformedSeq extends SeqFactory[TransformedSeq] {
  implicit def canBuildFrom[T]: CanBuildFrom[Coll, T, TransformedSeq[T]] =
    new GenericCanBuildFrom[T] {
      override def apply(from: Coll) = from match {
        case f: TransformedSeq[_] => new TransformedBuilder[T](f.observing)
      }
    }
  def newBuilder[T] = new TransformedBuilder[T](new Observing{})
  class TransformedBuilder[T](o: Observing)
  extends Builder[T, TransformedSeq[T]] {
    var list = List[T]()
    def result = new TransformedSeq[T] {
      val observing = o 
      lazy val underlying = list.reverse
    }
    def clear { list = Nil}
    def +=(elem: T) = {list ::= elem; this}
  }
}


