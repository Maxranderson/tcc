package sagres.model


case class TipoAcao(id: Option[Long], codigo: String, nome: String, var cancelled: Option[Long] = None)

object TiposAcao{

  val acoes = Seq(
    TipoAcao(Some(1), "0", "Operações Especiais", None),
    TipoAcao(Some(2), "1", "Projeto", None),
    TipoAcao(Some(3),"2", "Atividade", None)
  )

  def findUnique(id: String): Option[TipoAcao] = {
    acoes.find(_.codigo.contentEquals(id))
  }

}