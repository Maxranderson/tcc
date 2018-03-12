package tcc.validador

import com.sun.net.httpserver.Authenticator.Success
import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum
import sagres.model.{Acao, TipoErroImportacaoEnum}
import sagres.service.CaboBrancoService

import scala.util.{Failure, Success}

object ValidadorAcao extends Validador[Acao, ArquivoAcao]{

  object integridade {
    def unidadeGestoraTemSeisNumeros: (ArquivoAcao, MetaDadosValidacao) => Option[TipoErro] = {
      fazerRegra(TipoErroImportacaoEnum.ERROR, "algo"){
        (arquivo, dados) => arquivo.unidadeGestora.length != 6
      }
    }

    def naoPertenceMunicipio: (ArquivoAcao, MetaDadosValidacao) => Option[TipoErro] = {
      fazerRegra(TipoErroImportacaoEnum.ERROR, "Unidade Gestora não pertence ao município atual"){
        (arquivo, dados) => arquivo.unidadeGestora.length != 6
      }
    }

  }

  object externo {
    def existeUnidadeGestora: (ArquivoAcao, MetaDadosValidacao) => Option[TipoErro] = {
      fazerRegra(TipoErroImportacaoEnum.ERROR, "Unidade Gestora não existe em nosso sistema"){
        (arquivo, dados) =>
          CaboBrancoService.getJurisdicionadoByNumeroUG(arquivo.unidadeGestora) match {
            case Success(result) => result.contains(arquivo.unidadeGestora)
            case Failure(e) => false
          }
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

  override protected def gerarEtapas(anoCompetencia: Int): Seq[Seq[( ArquivoAcao, MetaDadosValidacao) => Option[TipoErro]]] = anoCompetencia match {
    case _ => List(List(integridade.unidadeGestoraTemSeisNumeros))
  }

  override protected def gerarEntidade(entidadeArquivo: ArquivoAcao, metaDadosValidacao: MetaDadosValidacao): Option[Acao] = {
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
                      )
