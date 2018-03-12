package tcc.validador

import java.sql.Date

import sagres.model.{ControleArquivo, TipoErroImportacaoEnum}
import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum

abstract class Validador[A, B] {

  protected def castEntidade(entidadeArquivo: B)(code: (B) => Option[TipoErro]): Option[TipoErro] = {
    entidadeArquivo match {
      case e: B => code(e)
      case _ => None
    }
  }

  protected def fazerRegra(tipo: TipoErroImportacaoEnum, mensagem: String)(decisao: (B, MetaDadosValidacao) => Boolean)(entidadeArquivo: B, dadosValidacao: MetaDadosValidacao): Option[TipoErro] = {
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

  protected def gerarEntidadeArquivo(linha: String, metaDados: MetaDadosValidacao): B

  protected def gerarEtapas(anoCompetencia: Int): Seq[Etapa[B]]

  protected def gerarEntidade(entidadeArquivo: B, metaDadosValidacao: MetaDadosValidacao): Option[A]

  protected def processarEtapas(entidadeArquivo: B, dadosValidacao: MetaDadosValidacao, etapas: Seq[Etapa[B]], errosAnteriores: Seq[TipoErro] = Seq[TipoErro]()): ResultadoValidacao[A] = {
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

case class Etapa[A](regras: Seq[Regra[A]], processador: (A, MetaDadosValidacao, Seq[Regra[A]]) => Seq[TipoErro] = Utils.processarEtapa)

case class Regra[A](validar: (A, MetaDadosValidacao) => Option[TipoErro])

package object Utils {
  def processarEtapa[A](entidadeArquivo: A, dadosValidacao: MetaDadosValidacao, etapa: Seq[Regra[A]]): Seq[TipoErro] = {
    etapa.flatMap(regra => regra.validar(entidadeArquivo, dadosValidacao))
  }
}