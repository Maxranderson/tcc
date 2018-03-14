package tcc.validador

import sagres.model.TipoErroImportacaoEnum
import sagres.service.CaboBrancoService

import scala.util.{Failure, Success}

trait unidadeGestoraRel {
  val codigoUnidadeGestora: String
}

object UnidadeGestoraRegras {

  object integridade {

    val todas = List(
      naoPertenceAoMuncipio,
      ehPrefeituraEhExcluida,
      naoEhPrefeituraEhUgDiferenteArquivo,
      naoContemUnidadeGestora
    )

    def naoPertenceAoMuncipio: Regra = {
      Regra(TipoErroImportacaoEnum.ERROR, "Unidade Gestora não pertence ao município atual"){
        (arquivo: unidadeGestoraRel, dados) =>
          arquivo.codigoUnidadeGestora.substring(3,6) != dados.unidadeGestoraArquivo.substring(3,6)
      }
    }

    def ehPrefeituraEhExcluida: Regra = {
      Regra(TipoErroImportacaoEnum.ERROR, "Prefeitura não pode enviar ações da Câmara ou de Consórcios"){
        (arquivo: unidadeGestoraRel, dados) =>
          dados.unidadeGestoraArquivo.substring(0,3) == "201" &&
            Seq("701", "201").contains(arquivo.codigoUnidadeGestora.substring(0,3))
      }
    }

    def naoEhPrefeituraEhUgDiferenteArquivo: Regra = {
      Regra(TipoErroImportacaoEnum.ERROR, "Unidade Gestora inválida"){
        (arquivo: unidadeGestoraRel, dados) =>
          dados.unidadeGestoraArquivo.substring(0,3) != "201" &&
            arquivo.codigoUnidadeGestora.substring(0,3) != dados.unidadeGestoraArquivo.substring(0,3)
      }
    }

    def naoContemUnidadeGestora: Regra = {
      Regra(TipoErroImportacaoEnum.ERROR, "Código da Unidade Gestora deve ser informado"){
        (arquivo: unidadeGestoraRel, dados) => arquivo.codigoUnidadeGestora.isEmpty
      }
    }

  }

  object externo {

    val todas = List(
      naoExisteEssaUnidadeGestora
    )

    def naoExisteEssaUnidadeGestora: Regra = {
      Regra(TipoErroImportacaoEnum.ERROR, "Unidade Gestora não existe em nosso sistema"){
        (arquivo: unidadeGestoraRel, dados) =>
          CaboBrancoService.getJurisdicionadoByNumeroUG(arquivo.codigoUnidadeGestora) match {
            case Success(result) => result.contains(arquivo.codigoUnidadeGestora)
            case Failure(e) => true
          }
      }
    }

  }

}
