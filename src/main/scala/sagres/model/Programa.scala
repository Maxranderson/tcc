package sagres.model

/**
  * Created by dformiga on 11/10/2016.
  */

import java.sql.Date
import java.util.Calendar

case class Programa (id:Option[Long],
                     unidadeGestora: String,
                     ano: Int,
                     codigoPrograma: String,
                     denominacaoPrograma: Option[String],
                     descricaoObjetivo: Option[String],
                     objetivoMilenio: Option[Int],
                     mesEnvio: Option[Int],
                     confirmado: Boolean = false,
                     createdAt: Date = new Date(Calendar.getInstance.getTime.getTime),
                     var cancelled: Option[Long] = None
                    ){
  def equalsTo(obj: Programa) = {
    obj.codigoPrograma == codigoPrograma &&
        obj.ano == ano &&
        obj.unidadeGestora == unidadeGestora
  }
}

object Programas {
  def findUnique(unidadeGestora: String, ano: Int, codigoPrograma: String): Option[Programa] = {
    None
  }
}