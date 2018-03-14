package tcc.validador

import java.sql.Date

import sagres.model.{ControleArquivo, TipoErroImportacaoEnum}
import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum

import scala.util.{Success, Try}

abstract class Validador[A] {

  private def linhaDiferenteDoControleArquivo: Regra = {
    Regra(TipoErroImportacaoEnum.ERROR, "Tamanho da linha inválido"){
      (arquivo: entidadeArquivo, dados) => dados.conteudoLinha.length != dados.controleArquivo.tamanhoLinha
    }
  }

  protected def gerarEntidadeArquivo(linha: String, metaDados: MetaDadosValidacao): entidadeArquivo

  protected def gerarEtapas(anoCompetencia: Int): Seq[Etapa]

  protected def gerarEntidade(entidadeArquivo: entidadeArquivo, metaDadosValidacao: MetaDadosValidacao): Option[A]

  protected def processarEtapas(entidadeArquivo: entidadeArquivo, dadosValidacao: MetaDadosValidacao, etapas: Seq[Etapa], errosAnteriores: Seq[TipoErro] = Seq[TipoErro]()): ResultadoValidacao[A] = {
    etapas match {
      case Nil => ResultadoValidacao(Option(errosAnteriores), gerarEntidade(entidadeArquivo, dadosValidacao))
      case etapa :: proximas =>
        val erros = etapa.processador(entidadeArquivo, dadosValidacao, etapa.regras)
        if (TipoErro.existeTipoErro(erros))
          ResultadoValidacao(Some(errosAnteriores ++: erros), None)
        else
          processarEtapas(entidadeArquivo, dadosValidacao,proximas, errosAnteriores ++: erros)
    }
  }

  protected def processoValidacao(linha: String, numeroLinha: Int, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo): ResultadoValidacao[A] = {
    val metaDados = MetaDadosValidacao(linha, numeroLinha, dataCompetencia, unidadeGestoraArquivo, controleArquivo)
    processarEtapas(gerarEntidadeArquivo(linha, metaDados), metaDados,Etapa(List(linhaDiferenteDoControleArquivo)) +: gerarEtapas(controleArquivo.ano), Seq[TipoErro]())
  }

  final def validar: (String, Int, Date, String, ControleArquivo) => ResultadoValidacao[A] = processoValidacao
}

final case class MetaDadosValidacao(conteudoLinha: String, numeroLinha: Int, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo)

trait entidadeArquivo

case class Etapa(regras: Seq[Regra], processador: (entidadeArquivo, MetaDadosValidacao, Seq[Regra]) => Seq[TipoErro] = Utils.processarEtapa)

case class Regra(validar: (entidadeArquivo, MetaDadosValidacao) => Option[TipoErro])

object Regra {

  def apply[T](tipo: TipoErroImportacaoEnum, mensagem: String)(decisao: (T, MetaDadosValidacao) => Boolean): Regra = new Regra(Utils.fazerRegra(tipo, mensagem)(decisao))
}

package object Utils {
  def castEntidadeTrait[T](entidade: entidadeArquivo)(code: (T) => Option[TipoErro]): Option[TipoErro] = {
    entidade match {
      case e: entidadeArquivo with T => code(e)
      case _ => None
    }
  }

  def fazerRegra[T](tipo: TipoErroImportacaoEnum, mensagem: String)(decisao: (T, MetaDadosValidacao) => Boolean)(entidadeArquivo: entidadeArquivo, dadosValidacao: MetaDadosValidacao): Option[TipoErro] = {
    Utils.castEntidadeTrait[T](entidadeArquivo) { entidadeArquivo =>
      if(decisao(entidadeArquivo, dadosValidacao)){
        Option(
          TipoErro(
            dadosValidacao.controleArquivo.codigoArquivo,
            dadosValidacao.numeroLinha,
            dadosValidacao.conteudoLinha,
            mensagem,
            tipo
          )
        )
      }else None
    }
  }

  def processarEtapa(entidadeArquivo: entidadeArquivo, dadosValidacao: MetaDadosValidacao, etapa: Seq[Regra]): Seq[TipoErro] = {
    etapa.flatMap(regra => regra.validar(entidadeArquivo, dadosValidacao))
  }

  def processarEtapaParalelo(entidadeArquivo: entidadeArquivo, dadosValidacao: MetaDadosValidacao, etapa: Seq[Regra]): Seq[TipoErro] = {
    etapa.par.flatMap(regra => regra.validar(entidadeArquivo, dadosValidacao)).toVector
  }
}