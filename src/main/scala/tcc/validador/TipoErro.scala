package tcc.validador

import sagres.model.TipoErroImportacaoEnum
import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum

sealed case class TipoErro(codigoArquivo: Int, numeroLinha: Int, conteudoLinha: String, msg: String, tipoErroImportacaoEnum: TipoErroImportacaoEnum)

object TipoErro {

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