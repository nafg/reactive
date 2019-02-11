package reactive

object Util {
  def debugString(o: AnyRef) = o.getClass.getName+"@"+Integer.toHexString(System.identityHashCode(o))+": "+o.toString

  /**
   * Given a Class instance, extract the original scala identifier name.
   * Class names can be of the form {{{[[abc] $] name [$ [nnn] ...]}}}
   */
  private[reactive] def scalaClassName(c: Class[_]) = {
    val name = c.getSimpleName
    val dropEnd = name.replaceAll("""(\$\d*)*\z""", "")
    dropEnd.toList.reverse.takeWhile('$' != _).reverse.mkString
  }
}
