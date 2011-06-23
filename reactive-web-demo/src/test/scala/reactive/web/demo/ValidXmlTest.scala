package reactive
package web
package demo


import java.io.{File, FileInputStream}
import scala.xml.XML
import net.liftweb.util._
import net.liftweb.common._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class ValidXmlTest extends FunSuite with ShouldMatchers {
  test("Valid xml") {
    def wellFormed(file: File): List[ParamFailure[File]]=>List[ParamFailure[File]] = {failures =>
      if(file.isDirectory && !List("reactive-core-api","reactive-web-api").contains(file.getName))
        file.listFiles.foldLeft(failures){(failure, file) => wellFormed(file)(failure)}
      else if(!file.isFile)
        failures
      else file.getName.split("\\.").last match {
        case "xml" =>
          try {
            XML loadFile file
            failures
          } catch {
            case e =>
              ParamFailure(e.getMessage,Full(e),Empty,file) :: failures
          }
        case "xml"|"xhtml"|"html"|"htm" =>
          PCDataXmlParser(new FileInputStream(file)) match {
            case Full(_) => failures
            case empty => ParamFailure("Parsing failed",Empty,Full(empty).collect{case f: Failure=>f}, file) :: failures
          }
        case _ => failures
      }
    }
    val results = wellFormed(new File("reactive-web-demo/src/main/webapp"))(Nil)
    results foreach println
    results should be ('empty)
  }
}

