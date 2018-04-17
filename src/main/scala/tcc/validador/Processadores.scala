package tcc.validador

import sagres.model.TipoErroImportacaoEnum
import tcc.validador.validadores.entidades.{Conversor, EntidadeArquivo}

import scala.util.{Failure, Success, Try}

object Processadores {

  def processarLinhasSequencial[A](tuplasLinhaIndice: Seq[(String, Int)], metaDadosSemLinha: MetaDados, gerarTuplaProcessadorEtapas: (MetaDados) => Try[(Conversor[A], Seq[Etapa])]): Try[ResultadosValidacao[A]] = {
    val tryTuplaProcessadorEtapas = gerarTuplaProcessadorEtapas(metaDadosSemLinha)
    def loop(linhasAhProcessar: Seq[(String, Int)], acumulador: (Seq[TipoErro], Option[Seq[A]]) = (Seq(), Option(Seq()))): Try[ResultadosValidacao[A]] = {
      linhasAhProcessar match {
        case Nil => Success(ResultadosValidacao(acumulador._1, acumulador._2))
        case tuplaLinhaIndice :: proximas =>
          val metaDados = metaDadosSemLinha.copy(conteudoLinha = tuplaLinhaIndice._1, numeroLinha = tuplaLinhaIndice._2+1)
          tryTuplaProcessadorEtapas match {
            case Failure(e) => Failure(e)
            case Success(tuplaProcessadorEtapas) =>
              processarEtapasSequencial(metaDados, tuplaProcessadorEtapas._1.fromFileLine(metaDados), tuplaProcessadorEtapas._1.entidadeArquivoParaEntidade, tuplaProcessadorEtapas._2.toList) match {
                case Failure(e) => Failure(e)
                case Success(resultados) => {
                  loop(proximas, (acumulador._1 ++: resultados._1,
                    acumulador._2.map{
                      seq => resultados._2 match {
                        case None => seq
                        case Some(entidade) => entidade +: seq
                      }
                    }
                  ))
                }
              }
          }
      }
    }
    loop(tuplasLinhaIndice)
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

  def processarSubEtapaSequencial(subEtapa: SubEtapa, entidadeArquivo: EntidadeArquivo, metaDados: MetaDados): Try[Seq[TipoErro]] = {
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

  def processarSubEtapa(subEtapa: SubEtapa, entidadeArquivo: EntidadeArquivo, metaDados: MetaDados): Try[Seq[TipoErro]] = Try {
    subEtapa.regras.flatMap(_.validar(entidadeArquivo, metaDados) match {
      case Failure(e) => throw e
      case Success(oe) => oe
    })
  }

  def processarEtapaSequencial(etapa: Etapa, entidadeArquivo: EntidadeArquivo, metaDados: MetaDados): Try[Seq[TipoErro]] = {
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
            case Success(erros) => loop(etapasSeguintes, erros ++: errosGerados)
            case Failure(e) => Failure(e)
          }
      }
    }
    loop(etapas, Seq()) map {
      case erros if TipoErro.existeTipoErro(erros) => (erros, None)
      case erros => (erros, Option(converter(entidadeArquivo, metaDados)))
    }
  }


  case class ExisteErroValidacaoException(erros: Seq[TipoErro]) extends Exception


}
