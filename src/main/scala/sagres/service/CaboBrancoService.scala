package sagres.service

import scala.util.{Success, Try}

object CaboBrancoService {

  val unidadesGestoras = Seq("201145")

  def getJurisdicionadoByNumeroUG(codigoUnidadeGestoraStr: String): Try[String] = {
    unidadesGestoras.find(_ == codigoUnidadeGestoraStr) match {
      case Some(codigo) => Success(codigo)
      case None => Success("")
    }
  }
}

