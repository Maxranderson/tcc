package tcc.validador

import sagres.model.Acao

object ValidadorAcao extends Validador[Acao, ArquivoAcao]{

  object integridade {
    def unidadeGestoraTemSeisNumeros(entidadeArquivo: ArquivoAcao, dadosValidacao: DadosValidacao) = {
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

  override protected def gerarEtapas(anoCompetencia: Int): Seq[Seq[( ArquivoAcao, DadosValidacao) => Option[TipoErro]]] = anoCompetencia match {
    case _ => List(List(integridade.unidadeGestoraTemSeisNumeros))
  }

  override protected def gerarEntidade(entidadeArquivo: ArquivoAcao): Option[Acao] = {
    Option(
      Acao(
        None,
        entidadeArquivo.ug,
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

case class ArquivoAcao(ug: String)
