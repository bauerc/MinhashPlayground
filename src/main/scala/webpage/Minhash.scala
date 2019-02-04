package webpage

import webpage.elements.Form

import scala.util.hashing.MurmurHash3

case class Minhash(
                     tokens: Set[(String, Boolean)],
                     jaccard: Double,
                     minhashes: List[(Int, Int, Boolean)],
                     bands: List[(Int, Int, Boolean)]
                   )


object Minhash {
  def apply(mhForm: Form): Minhash = {
    def minhash(tokens: Set[String], seed: Int): Int = {
      tokens.map(x => MurmurHash3.stringHash(x, seed)).min
    }

    def generateMinhashes(tokensA: Set[String], tokensB: Set[String]): List[(Int, Int, Boolean)] = {
      for {
        s <- 1 to mhForm.numMinhashes toList
      } yield {
        val m1 =minhash(tokensA, s)
        val m2 =minhash(tokensB, s)
        (m1, m2, m1 == m2)
      }
    }

    def band(minhashes: List[Int]): List[Int] = {
      minhashes.grouped(mhForm.numMinhashes/mhForm.numBands).map(x => MurmurHash3.seqHash(x)).toList
    }

    def generateBands(minhashes: List[(Int,Int,Boolean)]): List[(Int,Int,Boolean)] = {
      val bandA = band(minhashes.map(_._1))
      val bandB = band(minhashes.map(_._2))
      bandA.zip(bandB).map(x => (x._1, x._2, x._1 == x._2))
    }

    val tokenizer = {
      mhForm.tokenizer match {
        case "word" => (s: String) => {
          s.split(" ").toSet
        }
        case "shingle" => (s: String) => {
          s.sliding(3).toSet
        }
      }
    }
    val tokensA: Set[String] = tokenizer(mhForm.inputValueA)
    val tokensB: Set[String] = tokenizer(mhForm.inputValueB)
    val intersection = tokensA.intersect(tokensB)
    val tokens = tokensA.union(tokensB).map(x => (x, intersection.contains(x)))
    val jaccard: Double = tokens.count(_._2) * 1.0 / tokens.size
    val minhashes = generateMinhashes(tokensA,tokensB)
    val bands = generateBands(minhashes)
    new Minhash(tokens, jaccard, minhashes, bands)
  }



}
