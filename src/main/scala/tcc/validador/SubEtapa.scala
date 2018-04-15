package tcc.validador

import tcc.validador.validadores.entidades.EntidadeArquivo

import scala.util.{Failure, Success, Try}

protected[validador] case class SubEtapa(regras: Seq[Regra], processador: (SubEtapa, EntidadeArquivo, MetaDados) => Try[Seq[TipoErro]] = SubEtapa.processadores.processar)

protected[validador] object SubEtapa {
  object processadores {
    def processarSequencial(subEtapa: SubEtapa, entidadeArquivo: EntidadeArquivo, metaDados: MetaDados): Try[Seq[TipoErro]] = {
      def loop(regrasAtuais: Seq[Regra], acumulador: Seq[TipoErro] = Seq()): Try[Seq[TipoErro]] = {
        regrasAtuais match {
          case Nil => Success(acumulador)
          case atual :: proximas =>
            atual.validar(entidadeArquivo, metaDados) match {
              case Failure(e) => Failure(e)
              case Success(optionErro) =>
                optionErro match {
                  case None => loop(proximas, acumulador)
                  case Some(erro) => loop(proximas, erro +: acumulador)
                }
            }
        }
      }
      loop(subEtapa.regras)
    }

    def processar(subEtapa: SubEtapa, entidadeArquivo: EntidadeArquivo, metaDados: MetaDados): Try[Seq[TipoErro]] = Try {
      subEtapa.regras.flatMap(_.validar(entidadeArquivo, metaDados) match {
        case Failure(e) => throw e
        case Success(oe) => oe
      })
    }
  }
}
