package reactive

object Util {
  def debugString(o: AnyRef) = o.getClass.getName+"@"+Integer.toHexString(System.identityHashCode(o))+": "+o.toString
}
