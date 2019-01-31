package webpage
import org.scalajs.dom
import org.scalajs.dom.html
import scalatags.JsDom.all._
import webpage.elements.MinhashForm
import webpage.elements.MinhashOutputs

import scala.scalajs.js.annotation.JSExport

@JSExport
object Minhash extends{
  @JSExport
  def main(target: html.Div) ={

    // Create elements
    val submit = button(cls := "btn btn-primary", tpe := "button", "Submit").render

    // Actions
    submit.onclick = (e: dom.Event) => {
      require(MinhashForm.checkValidity)
      val processInputs = ProcessInputs(MinhashForm.getValues)
      MinhashOutputs.update(processInputs)

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
        MinhashForm.HTML,
        submit,
        MinhashOutputs.HTML
      ).render
    )

  }
}