package sagres.model

case class TipoNaturezaJuridica(id: Option[Long], nome: String, var cancelled: Option[Long] = None)

object TiposNaturezaJuridica {
  val tipos = Seq[TipoNaturezaJuridica]()
  def existsById(id: Long): Boolean = {
    tipos.exists(_.id == id)
  }
}