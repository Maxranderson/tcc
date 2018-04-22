package tcc.validador

import sagres.model.TipoErroImportacaoEnum
import tcc.validador.validadores.entidades.{Conversor, EntidadeArquivo}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Processadores {

  def processarLinhasSequencial[A](tuplasLinhaIndice: Seq[(String, Int)],
                                   metaDadosSemLinha: MetaDados,
                                   gerarTuplaProcessadorEtapas: (MetaDados) => Try[(Conversor[A], Seq[Etapa])]): Try[ResultadosValidacao[A]] = {
    val tuplaProcessadorEtapas = gerarTuplaProcessadorEtapas(metaDadosSemLinha) match {
      case Failure(e) => throw e
      case Success(s) => s
    }
    val conversor = tuplaProcessadorEtapas._1
    val etapas = tuplaProcessadorEtapas._2
    @tailrec
    def loop(linhasAhProcessar: Seq[(String, Int)], acumulador: (Seq[TipoErro], Option[Seq[A]]) = (Seq(), Option(Seq()))): Try[ResultadosValidacao[A]] = {
      linhasAhProcessar match {
        case Nil => Success(ResultadosValidacao(acumulador._1, acumulador._2))
        case tuplaLinhaIndice :: proximas =>
          val linha = tuplaLinhaIndice._1
          val numeroLinha = tuplaLinhaIndice._2+1
          val metaDados = metaDadosSemLinha.copy(conteudoLinha = linha, numeroLinha = numeroLinha)
          if(linha.length == metaDados.controleArquivo.tamanhoLinha){
            processarEtapasSequencial(metaDados, tuplaProcessadorEtapas._1.fromFileLine(metaDados), tuplaProcessadorEtapas._1.entidadeArquivoParaEntidade, tuplaProcessadorEtapas._2) match {
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
          }else {
            loop( proximas, (acumulador._1 ++: Seq(TipoErro(metaDados.controleArquivo.codigoArquivo, numeroLinha, linha, "Tamanho da linha inválida", TipoErroImportacaoEnum.ERROR)), None))
          }
      }
    }
    loop(tuplasLinhaIndice)
  }

  def processarLinhasParalelo[A](tuplasLinhaIndice: Seq[(String, Int)], metaDadosSemLinha: MetaDados, gerarTuplaProcessadorEtapas: (MetaDados) => Try[(Conversor[A], Seq[Etapa])]): Try[ResultadosValidacao[A]] = Try {
    val tuplaProcessadorEtapas = gerarTuplaProcessadorEtapas(metaDadosSemLinha) match {
      case Failure(e) => throw e
      case Success(s) => s
    }

    val conversor = tuplaProcessadorEtapas._1
    val etapas = tuplaProcessadorEtapas._2

    val result: (Seq[TipoErro], Option[Seq[A]]) = tuplasLinhaIndice.par.map(linhaIndice => {
      val linha = linhaIndice._1
      val numeroLinha = linhaIndice._2+1
      val metaDados = metaDadosSemLinha.copy(conteudoLinha = linha, numeroLinha = numeroLinha)
      if(linha.length == metaDados.controleArquivo.tamanhoLinha){
        processarEtapasSequencial(metaDados, conversor.fromFileLine(metaDados), conversor.entidadeArquivoParaEntidade, etapas) match {
          case Failure(e) => throw e
          case Success(resultado) => resultado
        }
      }else {
        (Seq(TipoErro(metaDados.controleArquivo.codigoArquivo, numeroLinha, linha, "Tamanho da linha inválida", TipoErroImportacaoEnum.ERROR)), None)
      }
    }).foldLeft((Seq.empty[TipoErro], Option(Seq.empty[A])))((atual, proximo) => (atual._1 ++: proximo._1,
      (atual._2, proximo._2) match {
        case (Some(a), Some(b)) => Option(b +: a)
        case _ => None
      }
    ))
    ResultadosValidacao(result._1, result._2)
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

  def processarSubEtapa(subEtapa: SubEtapa, entidadeArquivo: EntidadeArquivo, metaDados: MetaDados): Try[( Boolean, Seq[TipoErro])] = {
    subEtapa.regras.foldLeft(Try((false, Seq.empty[TipoErro]))){
      (acumulador, regra) =>
        acumulador.flatMap(
          tuplaBoolOpErros =>
            regra.validar(entidadeArquivo, metaDados)
              .map(opErro => {
                  val (comErro, erros) = tuplaBoolOpErros
                  opErro match {
                    case Some(erro) =>( opErro.exists(TipoErro.isError) || comErro, opErro.get +: erros)
                    case None => (comErro, erros)
                  }
                })
        )
    }
  }

  def processarEtapaSequencial(etapa: Etapa, entidadeArquivo: EntidadeArquivo, metaDados: MetaDados): Try[(Boolean, Seq[TipoErro])] = {
    def loop(subEtapasAtuais: Seq[SubEtapa], acumulador: (Boolean, Seq[TipoErro]) = (false, Seq())): Try[(Boolean, Seq[TipoErro])] = {
      subEtapasAtuais match {
        case Nil => Success(acumulador)
        case atual :: proximas =>
          atual.processador(atual, entidadeArquivo, metaDados) match {
            case Failure(e) => Failure(e)
            case Success(erros) => loop(proximas, (acumulador._1 || erros._1, acumulador._2 ++: erros._2))
          }
      }
    }
    loop(etapa.subEtapas)
  }

  def processarEtapasSequencial[A](metaDados: MetaDados, entidadeArquivo: EntidadeArquivo, converter: (EntidadeArquivo, MetaDados) => A, etapas: Seq[Etapa]): Try[(Seq[TipoErro], Option[A])] = {
    def loop(etapasAhProcessar: Seq[Etapa], errosGerados: (Boolean, Seq[TipoErro]) = (false, Seq())): Try[(Boolean, Seq[TipoErro])] = {
      etapasAhProcessar match {
        case Nil => Try(errosGerados)
        case etapaAtual :: etapasSeguintes =>
          etapaAtual.processador(etapaAtual, entidadeArquivo, metaDados) match {
            case Success(erros) if erros._1 => Try(( erros._1, erros._2))
            case Success(erros) => loop(etapasSeguintes, (errosGerados._1 || erros._1, errosGerados._2 ++: erros._2))
            case Failure(e) => Failure(e)
          }
      }
    }
    loop(etapas) map {
      case erros if erros._1 => (erros._2, None)
      case erros => (erros._2, Option(converter(entidadeArquivo, metaDados)))
    }
  }


  case class ExisteErroValidacaoException(erros: Seq[TipoErro]) extends Exception


}
