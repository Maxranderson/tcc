package tcc.validador

import sagres.model.Acao

object ValidadorAcao extends Validador[Acao]{

  object integridade {
    def unidadeGestoraTemSeisNumeros(entidadeArquivo: ArquivoAcao, dadosValidacao: MetaDadosValidacao) = {
      if(entidadeArquivo.ug.length != 6)
        Option(
          Erro(
            dadosValidacao.controleArquivo.codigoArquivo,
            dadosValidacao.numeroLinha,
            dadosValidacao.conteudoLinha,
            "Unidade Gestora Deve conter seis dígitos"
          )
        )
      else None
    }
  }

  override protected def gerarEntidadeArquivo(linha: String): ArquivoAcao = ArquivoAcao(linha.substring(0,6))

  override protected def gerarEtapas(anoCompetencia: Int): Seq[Seq[( ArquivoAcao, MetaDadosValidacao) => Option[TipoErro]]] = anoCompetencia match {
    case _ => List(List(integridade.unidadeGestoraTemSeisNumeros))
  }

  override protected def gerarEntidade(entidadeArquivo: entidadeArquivo, metaDadosValidacao: MetaDadosValidacao): Option[Acao] = {
    entidadeArquivo match {
      case entidadeArquivo: ArquivoAcao =>
        Option(
          Acao(
            None,
            entidadeArquivo.unidadeGestora,
            2018,
            "algo",
            None,
            None,
            None,
            None,
            2
          )
        )
    }
  }
}

case class ArquivoAcao(unidadeGestora: String, codigoAcao: String, denominacaoAcao: String, tipoAcao: String, descricaoMeta: String, unidadeMedida: String) extends entidadeArquivo
