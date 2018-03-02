package tcc.validador

import java.sql.Date

import sagres.model.{ControleArquivo, TipoErroImportacaoEnum}
import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum

abstract class Validador[A, B]{

  abstract protected def gerarEntidadeArquivo(linha: String): B

  abstract protected def gerarEtapas(anoCompetencia: Int): Seq[Seq[( B, DadosValidacao) => Option[TipoErro]]]

  abstract protected def gerarEntidade(entidadeArquivo: B): Option[A]

  protected def processarEtapa(entidadeArquivo: B, dadosValidacao: DadosValidacao, etapa: Seq[( B, DadosValidacao) => Option[TipoErro]]): Seq[TipoErro] = {
    etapa.flatMap(regra => regra(entidadeArquivo))
  }

  protected def processarEtapas(entidadeArquivo: B, dadosValidacao: DadosValidacao, etapas: Seq[Seq[( B, DadosValidacao) => Option[TipoErro]]], errosAnteriores: Seq[TipoErro] = Seq()): ResultadoValidacao[A] = {
    etapas match {
      case Nil => ResultadoValidacao(Option(errosAnteriores), gerarEntidade(entidadeArquivo))
      case etapa :: proximas =>
        val erros = processarEtapa(entidadeArquivo, dadosValidacao, etapa)
        if (TipoErro.existeTipoErro(erros))
          ResultadoValidacao(Some(errosAnteriores ++: erros), None)
        else
          processarEtapas(entidadeArquivo, dadosValidacao,proximas, errosAnteriores ++: erros)
    }
  }

  protected def processoValidacao(processadorEtapas: (B, DadosValidacao, Seq[Seq[( B, DadosValidacao) => Option[TipoErro]]], Seq[TipoErro]) => ResultadoValidacao[A])
                                 (linha: String, numeroLinha: Int, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo): ResultadoValidacao[A] = {
    val entidadeArquivo = gerarEntidadeArquivo(linha)
    processadorEtapas(entidadeArquivo, DadosValidacao(linha, numeroLinha, dataCompetencia, unidadeGestoraArquivo, controleArquivo),gerarEtapas(controleArquivo.ano))
  }

  final def validar: (String, Int, Date, String, ControleArquivo) => ResultadoValidacao[A] = processoValidacao(processarEtapas)
}

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

sealed abstract class ResultadoValidacao[A]

case class ResultadoErro[A](erros: Seq[TipoErro]) extends ResultadoValidacao[A]

case class ResultadoAviso[A](erros: Seq[TipoErro], entidade: A) extends ResultadoValidacao[A]

case class ResultadoSucesso[A](entidade: A) extends ResultadoValidacao[A]

object ResultadoValidacao {
  def apply[A](errosOption: Option[Seq[TipoErro]], entidadeOption: Option[A]): ResultadoValidacao[A] = {
    (errosOption, entidadeOption) match {
      case (Some(erros), None) => ResultadoErro(erros)
      case (Some(erros), Some(entidade)) => verificarCasoErroEntidade(erros, entidade)
      case (None, Some(entidade)) => ResultadoSucesso(entidade)
    }
  }

  private def verificarCasoErroEntidade[A](erros: Seq[TipoErro], entidade: A): ResultadoValidacao[A] = {
    if(TipoErro.existeTipoErro(erros)) ResultadoErro(erros)
    else erros match {
      case Nil => ResultadoSucesso(entidade)
      case _ => ResultadoAviso(erros, entidade)
    }
  }
}

final case class DadosValidacao(conteudoLinha: String, numeroLinha: Int, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo)
