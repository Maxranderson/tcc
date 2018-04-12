package tcc.validador

import sagres.model.{Acao, Acoes, TipoErroImportacaoEnum, TiposAcao}

object ValidadorAcao extends Validador[Acao]{



  override protected def gerarEntidadeArquivo(linha: String, metaDados: MetaDadosValidacao): ArquivoAcao = {
    val getCampo: String => String =  metaDados.controleArquivo.getCampo(linha, _)
    ArquivoAcao(
      getCampo("codigoUnidadeGestora"),
      getCampo("codigoAcao"),
      getCampo("denominacaoAcao"),
      getCampo("tipoAcao"),
      getCampo("descricaoMeta"),
      getCampo("unidadeMedida")
    )
  }

  override protected def gerarEtapas(anoCompetencia: Int): Seq[Etapa] = anoCompetencia match {
    case _ => List(
      Etapa(
        UnidadeGestoraRegras.externo.todas ++: integridade.todas
      ),
      Etapa(
        UnidadeGestoraRegras.externo.todas ++: externo.todas
      )
    )
  }

  override protected def gerarEntidade(entidadeArquivo: EntidadeArquivo, metaDadosValidacao: MetaDadosValidacao): Option[Acao] = {
    entidadeArquivo match {
      case entidadeArquivo: ArquivoAcao =>
        Option(
          Acao(
            None,
            entidadeArquivo.codigoUnidadeGestora,
            metaDadosValidacao.dataCompetencia.toLocalDate.getYear,
            entidadeArquivo.codigoAcao,
            Option(entidadeArquivo.denominacaoAcao),
            Option(entidadeArquivo.tipoAcao),
            Option(entidadeArquivo.unidadeMedida),
            Option(entidadeArquivo.descricaoMeta),
            1
          )
        )
    }
  }
}


