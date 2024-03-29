package sagres.model

import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum
import tcc.validador.TipoErro

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Used for form validation
  *
  * @param msg
  *            error message
  * @param msgs
  *             multiple error messages
  */
case class ValidationException(msg: String, msgs: ListBuffer[String] = ListBuffer[String]()) extends Exception
case class ValidationWarningException(msg: String, msgs: ListBuffer[String] = ListBuffer[String]()) extends Exception
case class TimeOutException(msg: String) extends Exception
case class ConfirmarEnvioException(msg: String) extends Exception
case class ValidacaoEmAndamentoException(msg: String) extends Exception
case class ResourceNotFoundException(msg: String) extends Exception
/**
  * Used for File upload validation
  *
  * @param msg
  *            error message
  * @param msgs
  *             multiple error messages
  */
case class ImportacaoException(msg: String, msgs: mutable.HashMap[Int, mutable.HashMap[TipoErroImportacaoEnum,ListBuffer[ErroImportacao]]] = mutable.HashMap[Int, mutable.HashMap[TipoErroImportacaoEnum,ListBuffer[ErroImportacao]]](), erroImportacaoBase: ErroImportacao) extends Exception {

  def adicionarErro(codigoArquivo: Int, numeroLinha: Int, conteudoLinha: String, msg: String, tipoErro: TipoErroImportacaoEnum): Unit = {
    if(!msgs.contains(codigoArquivo)) {
      msgs.+=((codigoArquivo, mutable.HashMap[TipoErroImportacaoEnum,ListBuffer[ErroImportacao]]()))
    }

    if(!msgs(codigoArquivo).contains(tipoErro)){
      msgs(codigoArquivo).+=((tipoErro,ListBuffer[ErroImportacao]()))
    }

    msgs(codigoArquivo)(tipoErro) += erroImportacaoBase.copy(
      codigoArquivo = Some(codigoArquivo),
      numeroLinha = Some(numeroLinha),
      conteudoLinha = Some(conteudoLinha),
      msgErro = Some(msg),
      tipoErro = Some(tipoErro))
  }

  def adicionarErros(erros: Seq[ErroImportacao]): Unit = {
    val primeiro :: proximos = erros
    adicionarErro(primeiro.codigoArquivo.get, primeiro.numeroLinha.get, primeiro.conteudoLinha.get, primeiro.msgErro.get, primeiro.tipoErro.get)
    msgs(primeiro.codigoArquivo.get)(primeiro.tipoErro.get) ++= proximos
  }

}