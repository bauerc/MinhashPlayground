package webpage

import webpage.elements.MinhashForm

class ProcessInputs(mhForm: MinhashForm) {

  val _tokens: List[(String, Boolean)] = tokenize(mhForm)
  val _minhashes: List[(Long, Long, Boolean)] = List.empty
  val _bands: List[(Long, Long, Boolean)] = List.empty

  def tokens = _tokens
  def minhashes = _minhashes
  def bands = _bands

  def tokenize(mhForm: MinhashForm): List[(String, Boolean)] = {
    val tokenizer = mhForm.tokenizer match {
      case "word" => (s: String) => {s.split(" ").toSet}
      case "shingle" => (s: String) => {s.sliding(3).toSet}
    }


  }

}
