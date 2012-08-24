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

  def render( path: String ) = {
    openTemplate( path ) match {
      case Full( ( code, fileName, fileExtension ) ) => {
        fileExtension match {
          case "scala" => renderCodeMirror( code, fileName, fileExtension )
          case "html" => renderCodeMirror( code, fileName, fileExtension )
          case _ => <pre> { code } </pre>
        }
      }
      case Failure( msg, _, _ ) => {
        <div class="template-error">
          <i class="icon-exclamation-sign"></i>
          {
          msg
          }
        </div>
      }
      case _ => {
        <div class="template-error">
          <i class="icon-exclamation-sign"></i>
          Empty
        </div>
      }
    }
  }

  def openTemplate( path: String ) =
  {
    for {
      fileName <- Full( path.split('/').last ) ?~ ( "cannot parse a filename: " + path )
      fileExtension <- Full( fileName.split('.').last ) ?~ ( "cannot parse a file extension: " + fileName )
      code <- LiftRules.loadResourceAsString( path ) ?~ ( "template: " + path + " not found" )
    } yield ( code, fileName, fileExtension )
  }

  def renderCodeMirror( code:String, fileName:String, fileExtension:String ) : Elem = {

    val guid = Helpers.nextFuncName

    val mode = fileExtension match {
      case "scala" => "text/x-scala"
      case "html" => "text/html"
    }

    <lift:children>
      <textarea id={guid}>{code}</textarea>
      <script>
        $(function(){{
        CodeMirror.fromTextArea( document.getElementById("{guid}"), {{
        lineNumbers: true,
        readOnly: true,
        mode: "{mode}",
        theme: "solarized-dark"
        }})
        }})
      </script>
      <label for={guid}>{ fileName }</label>
    </lift:children>
  }
}
