package tcc.validador

import tcc.validador.validadores.entidades.EntidadeArquivo

import scala.util.{Failure, Success, Try}

protected[validador] case class Etapa(subEtapas: Seq[SubEtapa], processador: (Etapa, EntidadeArquivo, MetaDados) => Try[Seq[TipoErro]] = Etapa.processadores.processarSequencial)

protected[validador] object Etapa {

  object processadores {
    def processarSequencial(etapa: Etapa, entidadeArquivo: EntidadeArquivo, metaDados: MetaDados): Try[Seq[TipoErro]] = {
      def loop(subEtapasAtuais: Seq[SubEtapa], acumulador: Seq[TipoErro] = Seq()): Try[Seq[TipoErro]] = {
        subEtapasAtuais match {
          case Nil => Success(acumulador)
          case atual :: proximas =>
            atual.processador(atual, entidadeArquivo, metaDados) match {
              case Failure(e) => Failure(e)
              case Success(erros) =>
                erros match {
                  case erros if TipoErro.existeTipoErro(erros) => Success(erros)
                  case erros => loop(proximas, acumulador ++ erros)
                }
            }
        }
      }
      loop(etapa.subEtapas)
    }

    def processarEtapasSequencial[A](metaDados: MetaDados, entidadeArquivo: EntidadeArquivo, converter: (EntidadeArquivo, MetaDados) => A, etapas: Seq[Etapa]): Try[(Seq[TipoErro], Option[A])] = {
      def loop(etapasAhProcessar: Seq[Etapa], errosGerados: Seq[TipoErro]): Try[Seq[TipoErro]] = {
        etapasAhProcessar match {
          case Nil => Try(errosGerados)
          case etapaAtual :: etapasSeguintes =>
            etapaAtual.processador(etapaAtual, entidadeArquivo, metaDados) match {
              case Success(erros) if TipoErro.existeTipoErro(erros) =>Try(erros)
              case Success(erros) => loop(etapasSeguintes, erros ++ errosGerados)
              case Failure(e) => Failure(e)
            }
        }
      }
      loop(etapas, Seq()) map {
        case erros if TipoErro.existeTipoErro(erros) => (erros, None)
        case erros => (erros, Option(converter(entidadeArquivo, metaDados)))
      }
    }
  }
}