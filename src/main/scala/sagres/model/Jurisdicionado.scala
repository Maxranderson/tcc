package sagres.model
case class Jurisdicionado(
                           id: Option[Long],
                           pessoaJuridica: Option[Long],
                           poder: Option[Int],
                           tipoJurisdicionado: Option[Long],
                           localidade: Option[Long],
                           nome: String,
                           codigoSagres: Option[String],
                           previdenciario: Option[Boolean],
                           municipioImportacao: Option[String],
                           var cancelled: Option[Long] = None) {

}

object Jurisdicionado {

  object UG {
    lazy val PREFEITURA = "201"
    lazy val CAMARA = "101"
    lazy val CONSORCIO = "701"
  }

  lazy val ugsOrcamento: Seq[String] = Seq[String](
    UG.PREFEITURA,UG.CAMARA,UG.CONSORCIO
  )

  def podeEnviarOrcamento(codigoUG: String): Boolean = {
    val tipoUG = codigoUG.substring(0,3)
    ugsOrcamento.contains(tipoUG)
  }

  def isPrefeitura(ug: String) = ug.substring(0,3) == "201"

}


object Poder extends Enumeration {
  type Poder = Value

  val EXECUTIVO = 1
  val LEGISLATIVO = 2
  val JUDICIONARIO = 3
  val MINISTERIO_PUBLICO = 4
  val TRIBUNAL_DE_CONTAS = 5

}