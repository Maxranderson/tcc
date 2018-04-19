package tcc.validador.validadores.regras

import sagres.model.{Acoes, TipoErroImportacaoEnum, TiposAcao}
import tcc.validador.{MetaDados, Regra}
import tcc.validador.validadores.entidades.ArquivoAcao

import scala.util.Try

protected[validadores] object RegrasAcao {
  object integridade {

    val todas = Seq(
      regraNaoContemCodigoAcao,
      codigoAcaoMenorQueQuatro,
      denominacaoMenorQueDez,
      ehPrefeituraEhExcluida
    )

    def naoContemCodigoAcao(arquivoAcao: ArquivoAcao, metaDados: MetaDados) = Try {
      arquivoAcao.codigoAcao.isEmpty
    }

    def regraNaoContemCodigoAcao: Regra = Regra(TipoErroImportacaoEnum.ERROR, "Código da ação deve ser informado.")(naoContemCodigoAcao)

    def codigoAcaoMenorQueQuatro: Regra = {
      Regra(TipoErroImportacaoEnum.ERROR, "Código da ação não deve ser inferior a 4 caracteres."){
        (arquivo: ArquivoAcao, dados) => Try(arquivo.codigoAcao.length < 4)
      }
    }

    def denominacaoMenorQueDez: Regra = {
      val minDescricao = 10
      Regra(TipoErroImportacaoEnum.ERROR, s"Descrição da ação não deve ser inferior a $minDescricao caracteres"){
        (arquivo: ArquivoAcao, dados) => Try(arquivo.denominacaoAcao.length < minDescricao)
      }
    }

    def ehPrefeituraEhExcluida: Regra = {
      Regra(TipoErroImportacaoEnum.ERROR, "Prefeitura não pode enviar ações da Câmara ou de Consórcios"){
        (arquivo: ArquivoAcao, dados) =>
          Try {
            dados.unidadeGestoraArquivo.substring(0,3) == "201" &&
              Seq("701", "301").contains(arquivo.codigoUnidadeGestora.substring(0,3))
          }
      }
    }
  }

  object externo {

    val todas = Seq(
      existeAcao,
      naoExisteTipoAcao
    )

    def existeAcao: Regra = {
      Regra(TipoErroImportacaoEnum.ERROR, "Ação já cadastrada no sistema"){
        (arquivo: ArquivoAcao, dados) =>Try(Acoes.findUnique(
          arquivo.unidadeMedida,
          arquivo.codigoAcao,
          dados.dataCompetencia.toLocalDate.getYear
        ).isDefined)
      }
    }

    def naoExisteTipoAcao: Regra = {
      Regra(TipoErroImportacaoEnum.ERROR, "Código do tipo da ação inválida."){
        (arquivo: ArquivoAcao, dados) => Try(TiposAcao.findUnique(arquivo.tipoAcao).isEmpty)
      }
    }

  }
}
