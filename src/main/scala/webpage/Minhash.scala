package webpage
import org.scalajs.dom
import org.scalajs.dom.html
import scalatags.JsDom.all._
import webpage.elements.MinhashForm
import webpage.elements.MinhashOutputs

import scala.scalajs.js.annotation.JSExport
import scala.util.hashing.MurmurHash3
import scala.math.{ceil, sqrt}

@JSExport
object Minhash extends{
  @JSExport
  def main(target: html.Div) ={

    //Limit seeds ot something sensible
    val seeds = 1 to 200 toList

    //Create HTML elements to use
    val inputBox1 = input(
      cls := "form-control",
      tpe :="text",
      placeholder:="Enter first string!"
    ).render


    val inputBox2 = input(
      cls := "form-control",
      tpe :="text",
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
      cls := "form-control",
      option(value := "word","Word"),
      option(value := "shingle","Shingle")
    ).render

    val jaccardOutput = p("").render

    val tokens = div().render

    val submit = button(cls := "btn btn-primary", tpe := "button", "Submit").render

    val minhashBox = div().render

    val bandBox = div().render

    val minhashForm =  form(
      div(cls := "container",
        div(cls := "row",
          div(cls := "col-sm", inputBox1),
          div(cls := "col-sm", inputBox2)
        ),
        div(cls := "row",
          div(cls := "col-sm", p("Choose your tokenizer")),
          div(cls := "col-sm", tokenizer)
        ),
        div(cls := "row",
          div(cls := "col-sm", p("Choose your number of Minhashes (1-200)")),
          div(cls := "col-sm", mhNum)
        ),
        div(cls := "row",
          div(cls := "col-sm", p("Choose your number of Bands")),
          div(cls := "col-sm", bandNum)
        )
      ),
      div(submit)
    ).render

    val outputBoxes = div(cls := "container",
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
    )

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
            cls :="list-inline-item list-group-item-success",
            t
          ) else li(
            cls := "list-inline-item",
            t
          )
        },
        cls := "list-inline"
      ).render
    }

    def generateMinhashes(s1: String, s2:String): List[(Long, Long, Boolean)] = {
      val lim = mhNum.value.toInt
      for {
        s <- seeds.take(lim)
      } yield {
        val m1 =minhash(s1, s)
        val m2 =minhash(s2, s)
        (m1, m2, m1 == m2)
      }
    }

    def renderMinhashes(s1: String, s2: String): html.Div = {
      val lim = mhNum.value.toInt
      val minhashes = generateMinhashes(s1,s2)
      val numTrue = minhashes.count(_._3)
      div(cls := "container",
        fontSize := "12px",
        if (numTrue > 0) h6("Its a 'match'!") else h6("Its not a 'match'!"),
        p("There were " + numTrue + " matching minhashes out of " + lim),
        dl(cls := "row",
          dt(cls := "col-sm-4", p("Minhash Collision Rate: ")),
          dd(cls := "col-sm-8", (numTrue * 1.0)/lim)
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

    def renderBands(): html.Div = {
      val b = bandNum.value.toInt
      val h = mhNum.value.toInt
      val minhashes = generateMinhashes(inputBox1.value, inputBox2.value)
      val minA = minhashes.map(_._1)
      val minB = minhashes.map(_._2)
      val bandsA = minA.grouped(h/b).map(x => MurmurHash3.seqHash(x))
      val bandsB = minB.grouped(h/b).map(x => MurmurHash3.seqHash(x))
      val bands = bandsA.zip(bandsB).toList
      val numTrue = bands.count(x => x._1 == x._2)
      div(cls := "container",
        fontSize := "12px",
        if (numTrue > 0) h6("Its a 'match'!") else h6("Its not a 'match'!"),
        p("There were " + numTrue + " matching bands out of " + b),
        dl(cls := "row",
          dt(cls := "col-sm-4", p("Band Collision Rate: ")),
          dd(cls := "col-sm-8", (numTrue * 1.0)/b)
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

    def minhash(s: String, seed: Int): Long = {
      tokenize(s).map(x => MurmurHash3.stringHash(x, seed)).min
    }

    // Actions
    submit.onclick = (e: dom.Event) => {
//      require(minhashForm.checkValidity())
      val mhForm = MinhashForm.getValues
      val processInputs = ProcessInputs(mhForm)
      MinhashOutputs.update(processInputs)
//      jaccardOutput.innerHTML = ""
//      jaccardOutput.appendChild(p(jaccard(mhForm.inputValueA, mhForm.inputValueB)).render)
//      tokens.innerHTML = ""
//      tokens.appendChild(renderTokens(mhForm.inputValueA, mhForm.inputValueB))
//      minhashBox.innerHTML = ""
//      minhashBox.appendChild(renderMinhashes(mhForm.inputValueA, mhForm.inputValueB))
//      bandBox.innerHTML = ""
//      bandBox.appendChild(renderBands())
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
//        minhashForm,
//        hr(style := "width:40%"),
//        similarityBox,
//        hr(style := "width:40%"),
//        outputBoxes
        MinhashOutputs.HTML
      ).render

    )


  }
}