package tcc.validador

import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum
import sagres.model.{Acao, TipoErroImportacaoEnum}
import sagres.service.CaboBrancoService

import scala.util.{Failure, Success}

object ValidadorAcao extends Validador[Acao]{

  object integridade {
    def unidadeGestoraTemSeisNumeros: (entidadeArquivo, MetaDadosValidacao) => Option[TipoErro] = {
      Utils.fazerRegra(TipoErroImportacaoEnum.ERROR, "algo"){
        (arquivo: ArquivoAcao, dados) => arquivo.codigoUnidadeGestora.length != 6
      }
    }

    def naoPertenceMunicipio: (entidadeArquivo, MetaDadosValidacao) => Option[TipoErro] = {
      Utils.fazerRegra(TipoErroImportacaoEnum.ERROR, "Unidade Gestora não pertence ao município atual"){
        (arquivo: ArquivoAcao, dados) => arquivo.codigoUnidadeGestora.length != 6
      }
    }

  }

  object externo {

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
    case _ => List(Etapa(List(Regra(integridade.unidadeGestoraTemSeisNumeros))), Etapa(List(Regra(UnidadeGestoraRegras.externo.existeUnidadeGestora))))
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
                      ) extends entidadeArquivo with unidadeGestoraRel

trait unidadeGestoraRel {
  val codigoUnidadeGestora: String
}
object UnidadeGestoraRegras {
  object integridade {

  }

  object externo {
    def existeUnidadeGestora: (entidadeArquivo, MetaDadosValidacao) => Option[TipoErro] = {
      Utils.fazerRegra(TipoErroImportacaoEnum.ERROR, "Unidade Gestora não existe em nosso sistema"){
        (arquivo: unidadeGestoraRel, dados) =>
          CaboBrancoService.getJurisdicionadoByNumeroUG(arquivo.codigoUnidadeGestora) match {
            case Success(result) => result.contains(arquivo.codigoUnidadeGestora)
            case Failure(e) => false
          }
      }
    }
  }
}
