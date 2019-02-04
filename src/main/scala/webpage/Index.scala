package webpage
import org.scalajs.dom
import org.scalajs.dom.html
import scalatags.JsDom.all._
import webpage.elements.Form
import webpage.elements.Outputs

import scala.scalajs.js.annotation.JSExport

@JSExport
object Index extends{
  @JSExport
  def render(target: html.Div) ={

    // Create elements
    val submit = button(cls := "btn btn-primary", tpe := "button", "Submit").render

    // Actions
    submit.onclick = (e: dom.Event) => {
      require(Form.checkValidity)
      val processInputs = Minhash(Form.getValues)
      Outputs.update(processInputs)

    }

    //Render target
    target.appendChild(
      div(
        cls :=   "text-center",
        h1(cls := "display-1", "Minhash"),
        p(cls := "lead",
          "Type here to fiddle with Minhash " +
            "between two strings!"
        ),
        Form.HTML,
        submit,
        Outputs.HTML
      ).render
    )

  }
}