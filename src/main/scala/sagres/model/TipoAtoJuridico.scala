package sagres.model

case class TipoAtoJuridico(id: Option[Long], nome: String, var cancelled: Option[Long] = None)

object TiposAtoJuridico {
  val tipos = Seq[TipoAtoJuridico]()
  def existsById(id: Long): Boolean = {
    tipos.exists(_.id == id)
  }
}
