package sagres.model


case class ControleArquivo(id: Option[Long],
                           codigoArquivo: Int,
                           ano: Int,
                           nomeArquivo: String,
                           tamanhoLinha: Int,
                           ativo: Boolean,
                           tipoSistema: Int,
                           ordem: Int,
                           layout: Option[String],
                           var cancelled: Option[Long] = None
                          ){

  def getCampo(linha: String, campo: String): String = {
    ""
  }

  def getTamanho(campo: String) : Int = {
    10
  }
}

object ControlesArquivo{

}