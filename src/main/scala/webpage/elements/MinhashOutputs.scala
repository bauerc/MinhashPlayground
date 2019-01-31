package webpage.elements

import org.scalajs.dom.html
import org.scalajs.dom.html.{Div, Paragraph}
import scalatags.JsDom
import scalatags.JsDom.all._
import webpage.ProcessInputs

object MinhashOutputs {

  val minhashBox: Div = div().render

  val bandBox: Div = div().render

  val jaccardOutput: Paragraph = p("").render

  val tokens: Div = div().render

  val outputBoxes: Div = div(cls := "container",
    div(cls := "row",
      div(cls := "col-sm",
        style := "padding-right:20px; border-right: 1px solid #ccc;",
        h4("Minhashes"),
        minhashBox
      ),
      div(cls := "col-sm",
        h4("Bands"),
        bandBox
      )
    )
  ).render

  val similarityBox = div(cls := "container",
    dl(cls := "row",
      dt(cls := "col-sm-4", p("Jaccard Similarity: ")),
      dd(cls := "col-sm-8", jaccardOutput),
      dt(cls := "col-sm-4", p("Tokens (First 20): ")),
      dd(cls := "col-sm-8", tokens)
    )
  ).render

  val HTML: Div = div(cls := "container",
    hr(style := "width:40%"),
    similarityBox,
    hr(style := "width:40%"),
    outputBoxes
  ).render




  def update(processInputs: ProcessInputs): Unit = {
    jaccardOutput.innerHTML = ""
    jaccardOutput.appendChild(p(processInputs.jaccard).render)
    tokens.innerHTML = ""
    tokens.appendChild(renderTokens(processInputs.tokens))
    minhashBox.innerHTML = ""
    minhashBox.appendChild(renderMinhashes(processInputs.minhashes))
    bandBox.innerHTML = ""
    bandBox.appendChild(renderBands(processInputs.bands))
  }

  def renderTokens(tokens: Set[(String, Boolean)]): html.UList = {
    ul(
      cls := "list-inline",
      for {
        t <- tokens.take(20).toList
      } yield {
        if (t._2) li(
          cls :="list-inline-item list-group-item-success",
          t._1
        ) else li(
          cls := "list-inline-item",
          t._1
        )
      }
    ).render
  }

  def renderMinhashes(minhashes: List[(Int, Int, Boolean)]): html.Div = {
    val numTrue = minhashes.count(_._3)
    div(cls := "container",
      fontSize := "12px",
      if (numTrue > 0) h6("Its a 'match'!") else h6("Its not a 'match'!"),
      p("There were " + numTrue + " matching minhashes out of " + minhashes.size),
      dl(cls := "row",
        dt(cls := "col-sm-4", p("Minhash Collision Rate: ")),
        dd(cls := "col-sm-8", (numTrue * 1.0)/minhashes.size)
      ),
      for {
        m <- minhashes
      } yield {
        if (m._3) {
          div(cls := "row",
            div(cls := "col-sm bg-success text-white", m._1),
            div(cls := "col-sm bg-success text-white", m._2)
          )
        } else {
          div(cls := "row",
            div(cls := "col-sm", m._1),
            div(cls := "col-sm", m._2)
          )
        }

      }
    ).render
  }

  def renderBands(bands: List[(Int, Int, Boolean)]): html.Div = {
    val numTrue = bands.count(_._3)
    div(cls := "container",
      fontSize := "12px",
      if (numTrue > 0) h6("Its a 'match'!") else h6("Its not a 'match'!"),
      p("There were " + numTrue + " matching bands out of " + b),
      dl(cls := "row",
        dt(cls := "col-sm-4", p("Band Collision Rate: ")),
        dd(cls := "col-sm-8", (numTrue * 1.0)/bands.size)
      ),
      for {
        x <- bands
      } yield {
        if (x._1 == x._2) {
          div(cls := "row",
            div(cls := "col-sm bg-success text-white", x._1),
            div(cls := "col-sm bg-success text-white", x._2)
          )
        } else {
          div(cls := "row",
            div(cls := "col-sm", x._1),
            div(cls := "col-sm", x._2)
          )
        }
      }
    ).render

  }
}
