package net.liftweb.doc.snippet

import scala.xml.Elem

import net.liftweb.common.Full
import net.liftweb.http.LiftRules
import net.liftweb.util.Helpers


object CodeInjection {
  def load(path: String) =
    for {
      fileName      <- Full( path.split('/').last )           ?~ ( "cannot parse a filename: " + path )
      fileExtension <- Full( fileName.split('.').last )       ?~ ( "cannot parse a file extension: " + fileName )
      code          <- LiftRules.loadResourceAsString( path ) ?~ ( "template: " + path + " not found" )
      _ = println(fileName)
    } yield renderCodeMirror(code, fileName, fileExtension)

  def renderCodeMirror(code:String, fileName:String, fileExtension:String): Elem = {
    val guid = Helpers.nextFuncName

    val mode = fileExtension match {
      case "scala" => "'text/x-scala'"
      case "html" => "'text/html'"
      case other => s"""{ name: "$other" }"""
    }

    <div data-lift="children">
      <label for={guid}>{ fileName }</label>
      <textarea id={guid}>{code.stripLineEnd}</textarea>
      <script>
        $(function(){{
          CodeMirror.fromTextArea( document.getElementById("{guid}"), {{
            lineNumbers: true,
            lineWrapping: true,
            readOnly: true,
            mode: {mode},
            theme: "solarized-dark"
          }})
        }})
      </script>
    </div>
  }
}
