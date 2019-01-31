package webpage.elements

import org.scalajs.dom
import org.scalajs.dom.html
import scalatags.JsDom.all._

import scala.math.sqrt

object MinhashForm {

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

  val HTML =  form(
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
    )
  ).render

  def renderBandOptions(): List[html.Option] = {
    val i = mhNum.value.toInt
    val j = sqrt(i).ceil.toInt
    val l = for (n <- 1 until j if i%n == 0) yield { List(n, i/n) }
    val divisors = l.toList.flatten.distinct.sorted
    divisors.map(d => option(value := d.toString, d.toString).render)
  }

  mhNum.onchange = (e: dom.Event) => {
    require(mhNum.checkValidity())
    bandNum.innerHTML = ""
    for {i <- renderBandOptions()} yield bandNum.appendChild(i)
  }
  def getValues(): MinhashForm = {
    MinhashForm(inputBox1.value, inputBox2.value, tokenizer.value, mhNum.value.toInt, bandNum.value.toInt)
  }



}

case class MinhashForm(inputValue1: String, inputValue2: String, tokenizer: String, numMinhashes: Int, numBands: Int)