package tcc.validador

import sagres.model.TipoErroImportacaoEnum
import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum

sealed abstract class TipoErro extends Exception {
  val codigoArquivo: Int
  val numeroLinha: Int
  val conteudoLinha: String
  val tipoErroImportacaoEnum: TipoErroImportacaoEnum
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