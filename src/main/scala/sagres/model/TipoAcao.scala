package sagres.model


case class TipoAcao(id: Option[Long], codigo: String, nome: String, var cancelled: Option[Long] = None)

object TiposAcao{

  def findUnique(id: String): Option[TipoAcao] ={
    Some(TipoAcao(None, "algo", "nome"))
  }

}