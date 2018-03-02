package tcc.validador

import sagres.model.TipoErroImportacaoEnum
import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum

sealed abstract class TipoErro extends Exception {
  abstract val codigoArquivo: Int
  abstract val numeroLinha: Int
  abstract val conteudoLinha: String
  abstract val tipoErroImportacaoEnum: TipoErroImportacaoEnum
}

object TipoErro {

  def existeTipoErro(lista: Seq[TipoErro]): Boolean = lista match {
    case Nil => false
    case head :: tail => head match {
      case _: Erro => true
      case _ => existeTipoErro(tail)
    }
  }
}

sealed case class Erro(codigoArquivo: Int, numeroLinha: Int, conteudoLinha: String, msg: String) extends TipoErro {
  val tipoErroImportacaoEnum: TipoErroImportacaoEnum = TipoErroImportacaoEnum.ERROR
}

sealed case class Aviso(codigoArquivo: Int, numeroLinha: Int, conteudoLinha: String, msg: String) extends TipoErro {
  val tipoErroImportacaoEnum: TipoErroImportacaoEnum = TipoErroImportacaoEnum.WARNING
}