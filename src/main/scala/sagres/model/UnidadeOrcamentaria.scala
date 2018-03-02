package sagres.model

import java.sql.Date
import java.util.Calendar

case class UnidadeOrcamentaria (id:Option[Long],
                                unidadeGestora: String,
                                ano: Int,
                                mesEnvio: Int,
                                codigoUnidOrcamentaria: String,
                                descricaoUnidOrcamentaria: Option[String],
                                nomeSecretario: Option[String],
                                cpfSecretario: Option[String],
                                atoJuridico: Option[String],
                                naturezaJuridica: Option[String],
                                confirmado: Boolean = false,
                                createdAt: Date = new Date(Calendar.getInstance.getTime.getTime),
                                var cancelled: Option[Long] = None
                               ){
  def equalsTo(uo: UnidadeOrcamentaria): Boolean = {
    // sempre se considera ANO, além dos campos considerados chave
    uo.unidadeGestora == unidadeGestora &&
    uo.codigoUnidOrcamentaria == codigoUnidOrcamentaria &&
    uo.ano == ano

  }
}

object UnidadesOrcamentarias{
  def findUnique(unidadeGestora: String, codigoUnidadeOrcamentaria: String, ano: Int): Option[UnidadeOrcamentaria] = None
}