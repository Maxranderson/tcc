package tcc.validador.validadores

import sagres.model.Acao
import tcc.validador.{Etapa, MetaDados, SubEtapa, Utils}
import tcc.validador.validadores.entidades.{ArquivoAcao, Conversor, EntidadeArquivo}
import tcc.validador.validadores.regras.{RegrasAcao, RegrasUnidadeGestoraRel}

import scala.util.{Failure, Success, Try}

protected[validador] object Validadores {

  def validarAcao(metaDados: MetaDados): Try[(Conversor[Acao], Seq[Etapa])] = {
    processarPartialFunctions(metaDados, Seq(default))
  }

  val default: PartialFunction[MetaDados, (Conversor[Acao], Seq[Etapa])] = {
    case meta =>
      (
        ArquivoAcao,
        Seq(
          Etapa(Seq(SubEtapa( RegrasUnidadeGestoraRel.integridade.todas ++ RegrasAcao.integridade.todas ))),
          Etapa(Seq(SubEtapa( RegrasUnidadeGestoraRel.externo.todas ++ RegrasAcao.externo.todas )))
        )
      )
  }

  def processarPartialFunctions[A](metaDados: MetaDados, seq: Seq[PartialFunction[MetaDados, (Conversor[A], Seq[Etapa])]]): Try[(Conversor[A], Seq[Etapa])] = {
    def loop(ahProcessar: Seq[PartialFunction[MetaDados, (Conversor[A], Seq[Etapa])]]): Try[(Conversor[A], Seq[Etapa])] = {
      ahProcessar match {
        case Nil => Failure(new Exception("O parâmetros não corresponderam a nenhuma função"))
        case atual :: proximas =>
          Try(atual(metaDados)) match {
            case Failure(_) => loop(proximas)
            case Success(result) => Success(result)
          }
      }
    }
    loop(seq)
  }
}
