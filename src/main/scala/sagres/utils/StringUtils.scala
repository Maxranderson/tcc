package sagres.utils

object StringUtils {

  /**
    * Converte um número em string com N zeros à esquerda
    * @param number Número a ser convertido
    * @param minLength Comprimento final da string (caso precise, a string é preenchida com 0)
    * @return número fornecido com o comprimento definido, preenchido com zeros
    */
  def fromInt(number: Int, minLength: Int): String = {
    var result = number.toString

    if (result.length < minLength) {
      val difference = minLength - result.length

      result = "0"*difference + result
    }

    result
  }

  implicit class CustomString(s: String) {
    /**
      * @param msg Mensagem a ser exibida caso a string seja vazia.
      * @return string normal ou a mensagem fornecida
      */
    def ifIsEmpty(msg: String): String = if(s.isEmpty) msg else s

    /**
      * Verifica se length é maior que zero
      * @return boolean
      */
    def isDefined: Boolean = s.length > 0
  }
}
