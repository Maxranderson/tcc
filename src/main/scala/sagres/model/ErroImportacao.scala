package sagres.model

import java.sql.Date
import java.util.Calendar

import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum

case class ErroImportacao (id: Option[Long],
                           unidadeGestora: String,
                           ano: Int,
                           mes: Int,
                           dia: Option[Int],
                           tipoSistema: Int,
                           codigoArquivo: Option[Int],
                           numeroLinha: Option[Int],
                           conteudoLinha: Option[String],
                           msgErro: Option[String],
                           tipoErro: Option[TipoErroImportacaoEnum],
                           createdAt: Date = new Date(Calendar.getInstance.getTime.getTime),
                           var cancelled: Option[Long] = None )



object ErrosImportacao{

}

object TipoErroImportacaoEnum extends Enumeration {
  type TipoErroImportacaoEnum = Value

  val WARNING = Value(1)
  val ERROR = Value(2)

}
