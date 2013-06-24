package net.liftweb.doc.snippet


/*
once again to lazy to add depedency
https://github.com/MasseGuillaume/lift-doc
*/

import net.liftweb.util.Helpers._
import net.liftweb.http.LiftRules
import net.liftweb.common.{ Failure, Full }

import xml.{NodeSeq, Elem}
import net.liftweb.util.Helpers


object CodeInjection
{

  def load( path: String ) = {

    for {
      fileName      <- Full( path.split('/').last )           ?~ ( "cannot parse a filename: " + path )
      fileExtension <- Full( fileName.split('.').last )       ?~ ( "cannot parse a file extension: " + fileName )
      code          <- LiftRules.loadResourceAsString( path ) ?~ ( "template: " + path + " not found" )
      _ = println(fileName)
    } yield renderCodeMirror( code, fileName, fileExtension )
  }

  def renderCodeMirror( code:String, fileName:String, fileExtension:String ) : Elem = {

    val guid = Helpers.nextFuncName

    val mode = fileExtension match {
      case "scala" => "'text/x-scala'"
      case "html" => "'text/html'"
      case other => s"{ name: $other }"
    }

    <lift:children>
      <label for={guid}>{ fileName }</label>
      <textarea id={guid}>{code}</textarea>
      <script>
        $(function(){{
        CodeMirror.fromTextArea( document.getElementById("{guid}"), {{
        lineNumbers: true,
        readOnly: true,
        mode: {mode},
        theme: "solarized-dark"
        }})
        }})
      </script>
    </lift:children>
  }
}
