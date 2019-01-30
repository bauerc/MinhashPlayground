package webpage
import org.scalajs.dom
import org.scalajs.dom.html
import scalatags.JsDom.all._

import scala.scalajs.js.annotation.JSExport
import scala.util.hashing.MurmurHash3
import scala.math.{sqrt, ceil}

@JSExport
object Minhash extends{
  @JSExport
  def main(target: html.Div) ={

    val seeds = 1 to 200 toList

    //Create HTML elements to use
    val box1 = input(
      `type`:="text",
      placeholder:="Enter first string!"
    ).render


    val box2 = input(
      `type`:="text",
      placeholder:="Enter second string!"
    ).render

    val mhNum = input(
      tpe := "number",
      cls := "form-control",
      value := "5",
      min := "1",
      max := "200"
    ).render

    val bandNum = select(
      cls := "form-control",
      option(value := "1", "1"),
      option(value := "5", "5")
    ).render

    val tokenizer = select(
      `class` := "form-control",
      option(value := "word","Word"),
      option(value := "shingle","Shingle")
    ).render

    val normalizer = select(
      `class` := "form-control",
      multiple,
      option(value := "lowercase","Lower Case"),
      option(value := "rmpunct","Remove Punctuation")
    ).render

    val output = p("").render

    val tokens = div().render

    val submit = button(cls := "btn btn-primary", tpe := "submit", "Submit").render

    val minhashBox = div().render

    //Functions to change the page
    def getTokenizer(): String => Set[String] = {
      tokenizer.value match {
        case "word" => (s: String) => {s.split(" ").toSet}
        case "shingle" => (s: String) => {s.sliding(3).toSet}
      }
    }

    def jaccard(s1: String, s2: String): Double = {
      val token1 = tokenize(s1.toLowerCase)
      val token2 = tokenize(s2.toLowerCase)
      (token1.intersect(token2).size * 1.0)/token1.union(token2).size
    }

    def tokenize(s: String): Set[String] = {
      getTokenizer()(s)
    }

    def renderTokens(s1: String, s2: String): html.UList = {
      val token1 = tokenize(s1.toLowerCase)
      val token2 = tokenize(s2.toLowerCase)
      val intersection = token1.intersect(token2)
      val union = token1.union(token2)
      ul(
        for {
          t <- union.toList.take(20)
        } yield {
          if (intersection.contains(t)) li(
            `class` :="list-inline-item list-group-item-success",
            t
          ) else li(
            `class` := "list-inline-item",
            t
          )
        },
        `class` := "list-inline"
      ).render
    }


    def renderMinhashes(s1: String, s2: String): html.Div = {
      val lim = mhNum.value.toInt
      val minhashes = for {
        s <- seeds.take(lim)
      } yield {
        val m1 =minhash(s1, s)
        val m2 =minhash(s2, s)
        (m1, m2, m1 == m2)
      }
      val numTrue = minhashes.count(_._3)
      div(`class` := "container",
        fontSize := "12px",
        if (numTrue > 0) h6("Its a 'match'!") else h6("Its not a 'match'!"),
        p("There were " + numTrue + " matching minhashes out of " + lim),
        dl(`class` := "row",
          dt(`class` := "col-sm-4", p("Minhash Jaccard Similarity: ")),
          dd(`class` := "col-sm-8", (numTrue * 1.0)/lim)
        ),
        //        a(
        //          cls := "btn btn-secondary",
        //          `type` := "button",
        //          href := "#collapseExample",
        //          attr("data-toggle") := "collapse",
        //          attr("aria-expanded") := "false",
        //          attr("aria-controls") := "collapseExample",
        //          "Open Minhashes"),
        //        div(
        //          cls := "collapse",
        //          id := "collapseExample",
        //          div(
        //            cls := "container",
        for {
          m <- minhashes
        } yield {
          if (m._3) {
            div(`class` := "row",
              div(`class` := "col-sm bg-success text-white", m._1),
              div(`class` := "col-sm bg-success text-white", m._2)
            )
          } else {
            div(`class` := "row",
              div(`class` := "col-sm", m._1),
              div(`class` := "col-sm", m._2)
            )
          }

        }
        //      )
        //        )
      ).render
    }

    def renderBandOptions(): List[html.Option] = {
      val i = mhNum.value.toInt
      val j = sqrt(i).ceil.toInt
      val l = for (n <- 1 until j if i%n == 0) yield { List(n, i/n) }
      val divisors = l.toList.flatten.distinct.sorted

      divisors.map(d => option(value := d.toString, d.toString).render)

    }

    def minhash(s: String, seed: Int): Long = {
      tokenize(s).map(x => MurmurHash3.stringHash(x, seed)).min
    }

    // Actions
    submit.onclick = (e: dom.Event) => {
      output.innerHTML = ""
      output.appendChild(p(jaccard(box1.value, box2.value)).render)
      tokens.innerHTML = ""
      tokens.appendChild(renderTokens(box1.value, box2.value))
      minhashBox.innerHTML = ""
      minhashBox.appendChild(renderMinhashes(box1.value, box2.value))
    }

    mhNum.onchange = (e: dom.Event) => {
      bandNum.innerHTML = ""
      for {i <- renderBandOptions()} yield bandNum.appendChild(i)
    }




    //Render target
    target.appendChild(
      div(
        `class` :=   "text-center",
        h1(`class` := "display-1", "Minhash"),
        p(`class` := "lead",
          "Type here to fiddle with Minhash " +
            "between two strings!"
        ),
        form(
          table(tbody(tr(td(box1), td(box2))), margin := "auto"), //TODO change to div
          div(`class` := "container",
            div(`class` := "row",
              div(`class` := "col-sm", p("Choose your tokenizer")),
              div(`class` := "col-sm", tokenizer)
            ),
              div(`class` := "row",
              div(`class` := "col-sm", p("Choose your number of Minhashes (1-200)")),
              div(`class` := "col-sm", mhNum)
            ),
            div(`class` := "row",
              div(`class` := "col-sm", p("Choose your number of Bands")),
              div(`class` := "col-sm", bandNum)
            )
          ),
          div(submit)
        ),
        div(`class` := "container",
          dl(`class` := "row",
            dt(`class` := "col-sm-4", p("Jaccard Similarity: ")),
            dd(`class` := "col-sm-8", output),
            dt(`class` := "col-sm-4", p("Tokens (First 20): ")),
            dd(`class` := "col-sm-8", tokens)
          )
        ),
        div(`class` := "container",
          div(`class` := "row",
            div(`class` := "col-sm",
              h4("Minhashes"),
              minhashBox
            ),
            div(`class` := "col-sm",
              h4("Bands")
            )
          )
        )
      ).render

    )


  }
}