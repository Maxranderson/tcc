package tcc.validador

import tcc.validador.entidades.EntidadeArquivo

import scala.util.{Failure, Success, Try}

case class Etapa(subEtapas: Seq[SubEtapa], processador: (EntidadeArquivo, MetaDadosValidacao, Seq[Regra]) => Try[Seq[TipoErro]] = Utils.processarEtapa)

object Etapa {

  object processadores {
    def processarEtapa(entidadeArquivo: EntidadeArquivo, dadosValidacao: MetaDadosValidacao, etapa: Seq[Regra]): Try[Seq[TipoErro]] = {
      def loop(etapa: Seq[Regra], acumulador: Seq[TipoErro] = Seq()): Try[Seq[TipoErro]] = {
        etapa match {
          case Nil => Success(acumulador)
          case head :: tail =>
            head.validar(entidadeArquivo, dadosValidacao) match {
              case Success(tipoErro) =>
                tipoErro match {
                  case Some(valor) => loop(tail, valor +: acumulador)
                  case None => loop(tail, acumulador)
                }
              case Failure(e) => Failure(e)
            }
        }
      }
      loop(etapa)
    }
  }

}