package tcc.validador

import java.sql.Date

import sagres.model.{ControleArquivo, TipoErroImportacaoEnum}
import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum

abstract class Validador[A] {

  protected def gerarEntidadeArquivo(linha: String, metaDados: MetaDadosValidacao): entidadeArquivo

  protected def gerarEtapas(anoCompetencia: Int): Seq[Seq[( entidadeArquivo, MetaDadosValidacao) => Option[TipoErro]]]

  protected def gerarEntidade(entidadeArquivo: entidadeArquivo, metaDadosValidacao: MetaDadosValidacao): Option[A]

  protected def processarEtapa(entidadeArquivo: entidadeArquivo, dadosValidacao: MetaDadosValidacao, etapa: Seq[( entidadeArquivo, MetaDadosValidacao) => Option[TipoErro]]): Seq[TipoErro] = {
    etapa.flatMap(regra => regra(entidadeArquivo, dadosValidacao))
  }

  protected def processarEtapas(entidadeArquivo: entidadeArquivo, dadosValidacao: MetaDadosValidacao, etapas: Seq[Seq[( entidadeArquivo, MetaDadosValidacao) => Option[TipoErro]]], errosAnteriores: Seq[TipoErro] = Seq[TipoErro]()): ResultadoValidacao[A] = {
    etapas match {
      case Nil => ResultadoValidacao(Option(errosAnteriores), gerarEntidade(entidadeArquivo, dadosValidacao))
      case etapa :: proximas =>
        val erros = processarEtapa(entidadeArquivo, dadosValidacao, etapa)
        if (TipoErro.existeTipoErro(erros))
          ResultadoValidacao(Some(errosAnteriores ++: erros), None)
        else
          processarEtapas(entidadeArquivo, dadosValidacao,proximas, errosAnteriores ++: erros)
    }
  }

  protected def processoValidacao(processadorEtapas: (entidadeArquivo, MetaDadosValidacao, Seq[Seq[( entidadeArquivo, MetaDadosValidacao) => Option[TipoErro]]], Seq[TipoErro]) => ResultadoValidacao[A])
                                 (linha: String, numeroLinha: Int, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo): ResultadoValidacao[A] = {
    val metaDados = MetaDadosValidacao(linha, numeroLinha, dataCompetencia, unidadeGestoraArquivo, controleArquivo)
    processadorEtapas(gerarEntidadeArquivo(linha, metaDados), metaDados,gerarEtapas(controleArquivo.ano), Seq[TipoErro]())
  }

  final def validar(linha: String, numeroLinha: Int, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo): ResultadoValidacao[A] = {
    processoValidacao(processarEtapas)(linha, numeroLinha, dataCompetencia, unidadeGestoraArquivo, controleArquivo)
  }
}

final case class MetaDadosValidacao(conteudoLinha: String, numeroLinha: Int, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo)

trait entidadeArquivo