package tcc.validador

import java.sql.Date

import sagres.model.{ControleArquivo, TipoErroImportacaoEnum}
import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum

abstract class Validador[A] {

  protected def gerarEntidadeArquivo(linha: String, metaDados: MetaDadosValidacao): entidadeArquivo

  protected def gerarEtapas(anoCompetencia: Int): Seq[Seq[Regra]]

  protected def gerarEntidade(entidadeArquivo: entidadeArquivo, metaDadosValidacao: MetaDadosValidacao): Option[A]

  protected def processarEtapa(entidadeArquivo: entidadeArquivo, dadosValidacao: MetaDadosValidacao, etapa: Seq[Regra]): Seq[TipoErro] = {
    etapa.flatMap(regra => regra.validar(entidadeArquivo, dadosValidacao))
  }

  protected def processarEtapas(entidadeArquivo: entidadeArquivo, dadosValidacao: MetaDadosValidacao, etapas: Seq[Seq[Regra]], errosAnteriores: Seq[TipoErro] = Seq[TipoErro]()): ResultadoValidacao[A] = {
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

  protected def processoValidacao(processadorEtapas: (entidadeArquivo, MetaDadosValidacao, Seq[Seq[Regra]], Seq[TipoErro]) => ResultadoValidacao[A])
                                 (linha: String, numeroLinha: Int, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo): ResultadoValidacao[A] = {
    val metaDados = MetaDadosValidacao(linha, numeroLinha, dataCompetencia, unidadeGestoraArquivo, controleArquivo)
    processadorEtapas(gerarEntidadeArquivo(linha, metaDados), metaDados,gerarEtapas(controleArquivo.ano), Seq[TipoErro]())
  }

  protected def processoValidacaoNormal: (String, Int, Date, String, ControleArquivo) => ResultadoValidacao[A] = processoValidacao(processarEtapas)

  final def validar: (String, Int, Date, String, ControleArquivo) => ResultadoValidacao[A] = processoValidacaoNormal
}

final case class MetaDadosValidacao(conteudoLinha: String, numeroLinha: Int, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo)

trait entidadeArquivo

case class Etapa(regras: Seq[Regra], processador: (entidadeArquivo, MetaDadosValidacao, Seq[Regra]) => Seq[Option[TipoErro]])

case class Regra(validar: (entidadeArquivo, MetaDadosValidacao) => Option[TipoErro])