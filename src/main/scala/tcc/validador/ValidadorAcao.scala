package tcc.validador

import sagres.model.{Acao, Acoes, TipoErroImportacaoEnum, TiposAcao}

object ValidadorAcao extends Validador[Acao]{

  object integridade {

    val todas = List(
      naoContemCodigoAcao,
      codigoAcaoMenorQueQuatro,
      denominacaoMenorQueDez
    )

    def naoContemCodigoAcao: Regra = Regra(TipoErroImportacaoEnum.ERROR, "Código da ação deve ser informado."){
      (arquivo: ArquivoAcao, dados) => arquivo.codigoAcao.isEmpty
    }

    def codigoAcaoMenorQueQuatro: Regra = {
      Regra(TipoErroImportacaoEnum.ERROR, "Código da ação não deve ser inferior a 4 caracteres."){
        (arquivo: ArquivoAcao, dados) => arquivo.codigoAcao.length < 4
      }
    }

    def denominacaoMenorQueDez: Regra = {
      val minDescricao = 10
      Regra(TipoErroImportacaoEnum.ERROR, s"Descrição da ação não deve ser inferior a $minDescricao caracteres"){
        (arquivo: ArquivoAcao, dados) => arquivo.denominacaoAcao.length < minDescricao
      }
    }
  }

  object externo {

    val todas = List(
      existeAcao,
      naoExisteTipoAcao
    )

    def existeAcao: Regra = {
      Regra(TipoErroImportacaoEnum.ERROR, "Ação já cadastrada no sistema"){
        (arquivo: ArquivoAcao, dados) => Acoes.findUnique(
          arquivo.unidadeMedida,
          arquivo.codigoAcao,
          dados.dataCompetencia.toLocalDate.getYear
        ).isDefined
      }
    }

    def naoExisteTipoAcao: Regra = {
      Regra(TipoErroImportacaoEnum.ERROR, "Código do tipo da ação inválida."){
        (arquivo: ArquivoAcao, dados) => TiposAcao.findUnique(arquivo.tipoAcao).isEmpty
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

  override protected def gerarEntidade(entidadeArquivo: entidadeArquivo, metaDadosValidacao: MetaDadosValidacao): Option[Acao] = {
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

case class ArquivoAcao(
                        codigoUnidadeGestora: String,
                        codigoAcao: String,
                        denominacaoAcao: String,
                        tipoAcao: String,
                        descricaoMeta: String,
                        unidadeMedida: String
                      ) extends entidadeArquivo with UnidadeGestoraRel
