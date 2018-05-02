package tcc.validador

import sagres.model.{ErroImportacao, TipoErroImportacaoEnum}
import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum

sealed case class TipoErro(codigoArquivo: Int, numeroLinha: Int, conteudoLinha: String, msg: String, tipoErroImportacaoEnum: TipoErroImportacaoEnum) {
  def toErroImportacao(metaDados: MetaDados): ErroImportacao = {
    metaDados.erroBase.copy(
      codigoArquivo = Some(codigoArquivo),
      numeroLinha = Some(numeroLinha),
      conteudoLinha = Some(conteudoLinha),
      msgErro = Some(msg),
      tipoErro = Some(tipoErroImportacaoEnum)
    )
  }
}

protected[validador] object TipoErro {

  def existeTipoErro(lista: Seq[TipoErro]): Boolean = lista match {
    case Nil => false
    case head :: tail => head match {
      case erro if erro.tipoErroImportacaoEnum.equals(TipoErroImportacaoEnum.ERROR) => true
      case _ => existeTipoErro(tail)
    }
  }

  def isError(erro: TipoErro): Boolean = {
    erro.tipoErroImportacaoEnum.equals(TipoErroImportacaoEnum.ERROR)
  }


}