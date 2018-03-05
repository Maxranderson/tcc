package tcc.validador

import java.sql.Date

import sagres.model.{ControleArquivo, TipoErroImportacaoEnum}
import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum

abstract class Validador[A] {

  protected def gerarEntidadeArquivo(linha: String, metaDados: MetaDadosValidacao): entidadeArquivo

  protected def gerarEtapas(anoCompetencia: Int): Seq[Seq[( entidadeArquivo, MetaDadosValidacao) => Option[TipoErro]]]

  protected def gerarEntidade(entidadeArquivo: entidadeArquivo, metaDadosValidacao: MetaDadosValidacao): Option[A]

  protected def processarEtapa(entidadeArquivo: entidadeArquivo, dadosValidacao: MetaDadosValidacao, etapa: Etapa): Seq[TipoErro] = {
    etapa.regras.flatMap(regra => regra(entidadeArquivo, dadosValidacao))
  }

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

  protected def processoValidacao(processadorEtapas: (entidadeArquivo, MetaDadosValidacao, Seq[Etapa], Seq[TipoErro]) => ResultadoValidacao[A])
                                 (linha: String, numeroLinha: Int, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo): ResultadoValidacao[A] = {
    val metaDados = MetaDadosValidacao(linha, numeroLinha, dataCompetencia, unidadeGestoraArquivo, controleArquivo)
    processadorEtapas(gerarEntidadeArquivo(linha, metaDados), metaDados,gerarEtapas(controleArquivo.ano), Seq[TipoErro]())
  }

  protected def processoValidacaoNormal: (String, Int, Date, String, ControleArquivo) => ResultadoValidacao[A] = processoValidacao(processarEtapas)

  final def validar: (String, Int, Date, String, ControleArquivo) => ResultadoValidacao[A] = processoValidacaoNormal
}

final case class MetaDadosValidacao(conteudoLinha: String, numeroLinha: Int, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo)

trait entidadeArquivo

case class Etapa(processador: (entidadeArquivo, MetaDadosValidacao, Etapa) => Seq[TipoErro], regras: Seq[( entidadeArquivo, MetaDadosValidacao) => Option[TipoErro]])