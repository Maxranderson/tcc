package tcc

import java.nio.charset.Charset

import sagres.model.{Acao, ControleArquivo}
import sagres.utils.DateUtils
import tcc.validador._

import scala.io.{Codec, Source}

object Teste extends App {

  val nomeArquivo = "201095012018Acao.txt"
  val ugArquivo = nomeArquivo.substring(0, 6)
  implicit val codec: Codec = Codec(Charset.forName("UTF-8"))
  val linhas = Source.fromResource("201095012018Acao.txt").getLines().toList
  implicit val controle: ControleArquivo = ControleArquivo(None, 10, 2018, "Acao", 120, ativo = true, 0, 2, None)

  val result = Metricas.tempoExecucaoComResultado {

    DateUtils.stringToSqlDate(nomeArquivo.substring(6, 12)) match {

      case Some(dataCompetencia) =>
        linhas.zipWithIndex.foldLeft( (List[TipoErro](), List[Acao]()) ) ( (tuplaListaErroEntidade, tupla) => {

          ValidadorAcao.validar(tupla._1, tupla._2 + 1, dataCompetencia, ugArquivo, controle) match {
            case ResultadoErro(erros) => (erros ++: tuplaListaErroEntidade._1, tuplaListaErroEntidade._2)
            case ResultadoAviso(erros, entidade) => (erros ++: tuplaListaErroEntidade._1, entidade +: tuplaListaErroEntidade._2)
            case ResultadoSucesso(entidade) => (tuplaListaErroEntidade._1, entidade +: tuplaListaErroEntidade._2)
          }
        })
      case None => (List[TipoErro](), List[Acao]())

    }

  }

}
