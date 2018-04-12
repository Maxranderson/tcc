package tcc.validador.regras

object Acao {
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
}
