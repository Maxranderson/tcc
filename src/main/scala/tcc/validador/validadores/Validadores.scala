package tcc.validador.validadores

import sagres.model.Acao
import tcc.validador.{Etapa, MetaDados, Processadores, SubEtapa}
import tcc.validador.validadores.entidades.{ArquivoAcao, Conversor}
import tcc.validador.validadores.regras.{RegrasAcao, RegrasUnidadeGestoraRel}

import scala.util.Try

protected[validador] object Validadores {

  def validarAcao(metaDados: MetaDados): Try[(Conversor[Acao], Seq[Etapa])] = {
    Processadores.processarPartialFunctions(metaDados, List(default))
  }

  val default: PartialFunction[MetaDados, (Conversor[Acao], Seq[Etapa])] = {
    case meta =>
      (
        ArquivoAcao,
        List(
          Etapa(List(SubEtapa( RegrasUnidadeGestoraRel.integridade.todas ++: RegrasAcao.integridade.todas ))),
          Etapa(List(SubEtapa( RegrasUnidadeGestoraRel.externo.todas ++: RegrasAcao.externo.todas )))
        )
      )
  }
}
