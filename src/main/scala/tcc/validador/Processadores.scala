package tcc.validador

import sagres.model.{ErroImportacao, TipoErroImportacaoEnum}
import tcc.Metricas
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
    def loop(linhasAhProcessar: Seq[(String, Int)], acumulador: ((Boolean, Seq[ErroImportacao]), Option[Seq[A]]) = ((false, Seq()), Option(Seq()))): Try[ResultadosValidacao[A]] = {
      linhasAhProcessar match {
        case Nil => Success(ResultadosValidacao(acumulador._1, acumulador._2))
        case tuplaLinhaIndice :: proximas =>
          val linha = tuplaLinhaIndice._1
          val numeroLinha = tuplaLinhaIndice._2+1
          val metaDados = metaDadosSemLinha.copy(conteudoLinha = linha, numeroLinha = numeroLinha)
          if(linha.length == metaDados.controleArquivo.tamanhoLinha){
            val resultados = aplicarRegraInfra(tuplasLinhaIndice, metaDados, tuplaLinhaIndice, (v: String) => conversor.fromFileLine(metaDados.copy(conteudoLinha = v))) {
              processarEtapasSequencial(metaDados, tuplaProcessadorEtapas._1.fromFileLine(metaDados), tuplaProcessadorEtapas._1.entidadeArquivoParaEntidade, tuplaProcessadorEtapas._2)
            }
             resultados match {
              case Failure(e) => Failure(e)
              case Success(resultados) => {
                val ((comErro, erros), entidadeOpt) = resultados
                loop(proximas, ((acumulador._1._1 || comErro, acumulador._1._2 ++: erros),
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
            loop( proximas, ((acumulador._1._1, acumulador._1._2 ++: Seq(TipoErro(metaDados.controleArquivo.codigoArquivo, numeroLinha, linha, "Tamanho da linha inválida", TipoErroImportacaoEnum.ERROR).toErroImportacao(metaDados))), None))
          }
      }
    }
    loop(tuplasLinhaIndice)
  }

  def aplicarRegraInfra[A](lista: Seq[(String, Int)], metaDados: MetaDados, tuplaLinhaIndiceAtual: (String, Int), converter: String => EntidadeArquivo)(bloco: => Try[((Boolean, Seq[ErroImportacao]), Option[A])]): Try[((Boolean,Seq[ErroImportacao]), Option[A])] = {
    def temDuplicidade(ent: EntidadeArquivo, converter: (String) => EntidadeArquivo, indiceAtual: Int, listaLinhaIndice: Seq[(String, Int)]): Boolean = {
//      def loop(listaAtual: Seq[(String, Int)], encontrou: Boolean = false): Boolean = {
//        listaAtual match {
//          case Nil => encontrou
//          case _ if encontrou => encontrou
//          case atual :: proximo if atual._2 < indiceAtual => loop(proximo, ent.ehIgual(converter(atual._1)))
//          case _ => encontrou
//        }
//      }
//      loop(listaLinhaIndice)
      listaLinhaIndice.splitAt(indiceAtual)._1.exists(p => converter(p._1).ehIgual(ent))
    }
    if(temDuplicidade(converter(tuplaLinhaIndiceAtual._1), converter, tuplaLinhaIndiceAtual._2, lista)){
      Try(((true, Seq(TipoErro(4,tuplaLinhaIndiceAtual._2+1,tuplaLinhaIndiceAtual._1, "Linha duplicada", TipoErroImportacaoEnum.ERROR).toErroImportacao(metaDados))), None))
    }
    else
      bloco
  }

  def processarLinhasParalelo[A](tuplasLinhaIndice: Seq[(String, Int)], metaDadosSemLinha: MetaDados, gerarTuplaProcessadorEtapas: (MetaDados) => Try[(Conversor[A], Seq[Etapa])]): Try[ResultadosValidacao[A]] = Try {
    val tuplaProcessadorEtapas = gerarTuplaProcessadorEtapas(metaDadosSemLinha) match {
      case Failure(e) => throw e
      case Success(s) => s
    }

    val conversor = tuplaProcessadorEtapas._1
    val etapas = tuplaProcessadorEtapas._2

    val result: ((Boolean, Seq[ErroImportacao]), Option[Seq[A]]) = tuplasLinhaIndice.par.map(linhaIndice => {
      val linha = linhaIndice._1
      val numeroLinha = linhaIndice._2+1
      val metaDados = metaDadosSemLinha.copy(conteudoLinha = linha, numeroLinha = numeroLinha)
      if(linha.length == metaDados.controleArquivo.tamanhoLinha){
        val resultados = aplicarRegraInfra(tuplasLinhaIndice, metaDados, linhaIndice, (v: String) => conversor.fromFileLine(metaDadosSemLinha.copy(conteudoLinha = v))) {
          processarEtapasSequencial(metaDados, conversor.fromFileLine(metaDados), conversor.entidadeArquivoParaEntidade, etapas)
        }
         resultados match {
          case Failure(e) => throw e
          case Success(resultado) => resultado
        }
      }else {
        ((true, Seq(TipoErro(metaDados.controleArquivo.codigoArquivo, numeroLinha, linha, "Tamanho da linha inválida", TipoErroImportacaoEnum.ERROR).toErroImportacao(metaDados))), None)
      }
    }).foldLeft(((false, Seq.empty[ErroImportacao]), Option(Seq.empty[A])))((atual, proximo) => (( atual._1._1 || proximo._1._1 , atual._1._2 ++: proximo._1._2),
      (atual._2, proximo._2) match {
        case (Some(a), Some(b)) => Option(b +: a)
        case _ => None
      }
    ))
    ResultadosValidacao(result._1, result._2)
  }

//  def processarSubEtapaSequencial(subEtapa: SubEtapa, entidadeArquivo: EntidadeArquivo, metaDados: MetaDados): Try[Seq[ErroImportacao]] = {
//    def loop(regrasAtuais: Seq[Regra], acumulador: Seq[ErroImportacao] = Seq()): Try[Seq[ErroImportacao]] = {
//      regrasAtuais match {
//        case Nil => Success(acumulador)
//        case atual :: proximas =>
//          atual.validar(entidadeArquivo, metaDados) match {
//            case Failure(e) => Failure(e)
//            case Success(optionErro) =>
//              optionErro match {
//                case None => loop(proximas, acumulador)
//                case Some(erro) => loop(proximas, erro +: acumulador)
//              }
//          }
//      }
//    }
//    loop(subEtapa.regras)
//  }

  def processarSubEtapa(subEtapa: SubEtapa, entidadeArquivo: EntidadeArquivo, metaDados: MetaDados): Try[( Boolean, Seq[ErroImportacao])] = {
    subEtapa.regras.foldLeft(Try((false, Seq.empty[ErroImportacao]))){
      (acumulador, regra) =>
        acumulador.flatMap(
          tuplaBoolOpErros =>
            regra.validar(entidadeArquivo, metaDados)
              .map(opErro => {
                  val (comErro, erros) = tuplaBoolOpErros
                  opErro.map(err => (TipoErro.isError(err) || comErro, err.toErroImportacao(metaDados) +: erros)).getOrElse((comErro, erros))
                })
        )
    }
  }

  def processarEtapaSequencial(etapa: Etapa, entidadeArquivo: EntidadeArquivo, metaDados: MetaDados): Try[(Boolean, Seq[ErroImportacao])] = {
    def loop(subEtapasAtuais: Seq[SubEtapa], acumulador: (Boolean, Seq[ErroImportacao]) = (false, Seq())): Try[(Boolean, Seq[ErroImportacao])] = {
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

  def processarEtapasSequencial[A](metaDados: MetaDados, entidadeArquivo: EntidadeArquivo, converter: (EntidadeArquivo, MetaDados) => A, etapas: Seq[Etapa]): Try[((Boolean, Seq[ErroImportacao]), Option[A])] = {
    def loop(etapasAhProcessar: Seq[Etapa], errosGerados: (Boolean, Seq[ErroImportacao]) = (false, Seq())): Try[(Boolean, Seq[ErroImportacao])] = {
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
      case erros if erros._1 => (erros, None)
      case erros => (erros, Option(converter(entidadeArquivo, metaDados)))
    }
  }

}
