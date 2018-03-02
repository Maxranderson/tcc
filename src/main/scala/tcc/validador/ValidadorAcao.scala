package tcc.validador

import sagres.model.Acao

object ValidadorAcao extends Validador[Acao]{

  object integridade {
    def unidadeGestoraTemSeisNumeros(entidadeArquivo: entidadeArquivo, dadosValidacao: MetaDadosValidacao) = {
      entidadeArquivo match {
        case entidadeArquivo: ArquivoAcao =>
          if(entidadeArquivo.unidadeGestora.length != 6)
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
  }

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

  override protected def gerarEtapas(anoCompetencia: Int): Seq[Seq[( entidadeArquivo, MetaDadosValidacao) => Option[TipoErro]]] = anoCompetencia match {
    case _ => List(List(integridade.unidadeGestoraTemSeisNumeros))
  }

  override protected def gerarEntidade(entidadeArquivo: entidadeArquivo, metaDadosValidacao: MetaDadosValidacao): Option[Acao] = {
    entidadeArquivo match {
      case entidadeArquivo: ArquivoAcao =>
        Option(
          Acao(
            None,
            entidadeArquivo.unidadeGestora,
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

case class ArquivoAcao(
                        unidadeGestora: String,
                        codigoAcao: String,
                        denominacaoAcao: String,
                        tipoAcao: String,
                        descricaoMeta: String,
                        unidadeMedida: String
                      ) extends entidadeArquivo
