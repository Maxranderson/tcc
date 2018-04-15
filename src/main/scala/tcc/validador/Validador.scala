package tcc.validador

import java.io.File
import java.sql.Date

import sagres.model.{Acao, ControleArquivo}
import tcc.Metricas
import tcc.validador.validadores.Validadores
import tcc.validador.validadores.entidades.{Conversor, EntidadeArquivo}

import scala.collection.parallel.ParSeq
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Validador {

  def validarAcaoFromFile(file: File,
                          dataCompetencia: Date,
                          unidadeGestoraArquivo: String,
                          controleArquivo: ControleArquivo): Try[ResultadosValidacao[Acao]] = {
    Utils.validarFromFile(file, dataCompetencia, unidadeGestoraArquivo, controleArquivo, Validadores.validarAcao)
  }

}

final case class MetaDados(conteudoLinha: String, numeroLinha: Int, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo)

protected[validador] object Utils {
  def castEntidadeTrait[T](entidade: EntidadeArquivo)(code: (T) => Try[Option[TipoErro]]): Try[Option[TipoErro]] = {
    entidade match {
      case e: EntidadeArquivo with T => code(e)
      case _ => Failure(new Exception("Não foi possivel fazer o cast da entidade"))
    }
  }

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
              Etapa.processadores.processarEtapasSequencial(metaDados, tuplaProcessadorEtapas._1.fromFileLine(metaDados), tuplaProcessadorEtapas._1.entidadeArquivoParaEntidade, tuplaProcessadorEtapas._2.toList) match {
                case Failure(e) => Failure(e)
                case Success(resultados) => {
                  loop(proximas, (acumulador._1 ++ resultados._1,
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

  def validarFromFile[A](file: File, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo, gerarTuplaProcessadorEtapas: (MetaDados) => Try[(Conversor[A], Seq[Etapa])]): Try[ResultadosValidacao[A]] = {
    Utils.processarLinhasSequencial(Source.fromFile(file, "UTF-8").getLines().toList.zipWithIndex, MetaDados("", 0, dataCompetencia, unidadeGestoraArquivo, controleArquivo), gerarTuplaProcessadorEtapas)
  }
}