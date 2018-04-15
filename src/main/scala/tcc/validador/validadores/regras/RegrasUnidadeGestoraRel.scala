package tcc.validador.validadores.regras

import sagres.model.TipoErroImportacaoEnum
import sagres.service.CaboBrancoService
import tcc.validador.Regra
import tcc.validador.validadores.entidades.UnidadeGestoraRel

import scala.util.Try

protected[validadores]object RegrasUnidadeGestoraRel {

  object integridade {

    val todas = List(
      naoPertenceAoMuncipio,
      naoEhPrefeituraEhUgDiferenteArquivo,
      naoContemUnidadeGestora
    )

    def naoPertenceAoMuncipio: Regra = {
      Regra(TipoErroImportacaoEnum.ERROR, "Unidade Gestora não pertence ao município atual"){
        (arquivo: UnidadeGestoraRel, dados) =>
          Try(arquivo.codigoUnidadeGestora.substring(3,6) != dados.unidadeGestoraArquivo.substring(3,6))
      }
    }

    def naoEhPrefeituraEhUgDiferenteArquivo: Regra = {
      Regra(TipoErroImportacaoEnum.ERROR, "Unidade Gestora inválida"){
        (arquivo: UnidadeGestoraRel, dados) =>
          Try {
            dados.unidadeGestoraArquivo.substring(0,3) != "201" &&
              arquivo.codigoUnidadeGestora.substring(0,3) != dados.unidadeGestoraArquivo.substring(0,3)
          }
      }
    }

    def naoContemUnidadeGestora: Regra = {
      Regra(TipoErroImportacaoEnum.ERROR, "Código da Unidade Gestora deve ser informado"){
        (arquivo: UnidadeGestoraRel, dados) => Try(arquivo.codigoUnidadeGestora.isEmpty)
      }
    }

  }

  object externo {

    val todas = List(
      naoExisteEssaUnidadeGestora
    )

    def naoExisteEssaUnidadeGestora: Regra = {
      Regra(TipoErroImportacaoEnum.ERROR, "Unidade Gestora não existe em nosso sistema"){
        (arquivo: UnidadeGestoraRel, dados) =>
          CaboBrancoService.getJurisdicionadoByNumeroUG(arquivo.codigoUnidadeGestora).map(result => !result.contains(arquivo.codigoUnidadeGestora))
      }
    }

  }

}
