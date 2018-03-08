package tcc.validador

import java.sql.Date

import sagres.model.{ControleArquivo, TipoErroImportacaoEnum}
import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum

abstract class Validador[A, B] {

  protected def castEntidade(entidadeArquivo: entidadeArquivo)(code: (B) => Option[TipoErro]): Option[TipoErro] = {
    entidadeArquivo match {
      case e: B => code(e)
      case _ => None
    }
  }

  protected def fazerRegra(tipo: TipoErroImportacaoEnum, mensagem: String)(decisao: (B, MetaDadosValidacao) => Boolean)(entidadeArquivo: entidadeArquivo, dadosValidacao: MetaDadosValidacao): Option[TipoErro] = {
    castEntidade(entidadeArquivo) { entidadeArquivo =>
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

  protected def gerarEntidadeArquivo(linha: String, metaDados: MetaDadosValidacao): entidadeArquivo

  protected def gerarEtapas(anoCompetencia: Int): Seq[Etapa]

  protected def gerarEntidade(entidadeArquivo: entidadeArquivo, metaDadosValidacao: MetaDadosValidacao): Option[A]

  protected def processarEtapas(entidadeArquivo: entidadeArquivo, dadosValidacao: MetaDadosValidacao, etapas: Seq[Etapa], errosAnteriores: Seq[TipoErro] = Seq[TipoErro]()): ResultadoValidacao[A] = {
    etapas match {
      case Nil => ResultadoValidacao(Option(errosAnteriores), gerarEntidade(entidadeArquivo, dadosValidacao))
      case etapa :: proximas =>
        val erros = etapa.processador(entidadeArquivo, dadosValidacao, etapa)
        if (TipoErro.existeTipoErro(erros))
          ResultadoValidacao(Some(errosAnteriores ++: erros), None)
        else
          processarEtapas(entidadeArquivo, dadosValidacao,proximas, errosAnteriores ++: erros)
    }
  }

  protected def processoValidacao(linha: String, numeroLinha: Int, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo): ResultadoValidacao[A] = {
    val metaDados = MetaDadosValidacao(linha, numeroLinha, dataCompetencia, unidadeGestoraArquivo, controleArquivo)
    processarEtapas(gerarEntidadeArquivo(linha, metaDados), metaDados,gerarEtapas(controleArquivo.ano), Seq[TipoErro]())
  }

  final def validar: (String, Int, Date, String, ControleArquivo) => ResultadoValidacao[A] = processoValidacao
}

final case class MetaDadosValidacao(conteudoLinha: String, numeroLinha: Int, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo)

trait entidadeArquivo

case class Etapa(regras: Seq[Regra], processador: (entidadeArquivo, MetaDadosValidacao, Seq[Regra]) => Seq[TipoErro] = Utils.processarEtapa)

case class Regra(validar: (entidadeArquivo, MetaDadosValidacao) => Option[TipoErro])

package object Utils {
  def processarEtapa(entidadeArquivo: entidadeArquivo, dadosValidacao: MetaDadosValidacao, etapa: Seq[Regra]): Seq[TipoErro] = {
    etapa.flatMap(regra => regra.validar(entidadeArquivo, dadosValidacao))
  }
}