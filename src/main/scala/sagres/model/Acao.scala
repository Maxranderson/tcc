package sagres.model

/**
  * Created by dformiga on 13/10/2016.
  */

import java.sql.Date
import java.util.Calendar

case class Acao (id:Option[Long],
                 unidadeGestora: String,
                 ano: Int,
                 codigo: String,
                 descricao: Option[String],
                 tipoAcao: Option[String],
                 descricaoUnidade: Option[String],
                 descricaoMeta: Option[String],
                 mesEnvio: Int,
                 confirmado: Boolean = false,
                 createdAt: Date = new Date(Calendar.getInstance.getTime.getTime),
                 var cancelled: Option[Long] = None){

  def equalsTo(a: Acao): Boolean = {
    codigo == a.codigo &&
        unidadeGestora == a.unidadeGestora
  }

}

object Acoes {
  def findUnique(unidadeGestora: String, codigoAcao: String, ano: Int): Option[Acao] = {
    None
  }
}