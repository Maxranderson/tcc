import java.io.File
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Paths}

import sagres.model._
import sagres.utils.DateUtils
import sagres.validator.AcaoFileLineValidator
import tcc.Metricas

import scala.collection.mutable.ListBuffer
import scala.io.{Codec, Source}

object RotinaOOTeste extends App {

  val nomeArquivo = "201095012018Acao.txt"
  val ugArquivo = nomeArquivo.substring(0, 6)
  implicit val codec: Codec = Codec(Charset.forName("UTF-8"))
  val arquivo = Source.fromResource(nomeArquivo)
  val listaArquivo = arquivo.getLines.toList
  implicit val controle: ControleArquivo = ControleArquivo(None, 10, 2018, "Acao", 287, ativo = true, 0, 2, None)
  implicit val erros: ImportacaoException = ImportacaoException("Falha na importação", erroImportacaoBase = ErroImportacao(None, "201095", 2018, 1, None, 1, None, None, None, None, None))
  val dataCompetenciaArquivo = DateUtils.stringToSqlDate(nomeArquivo.substring(6, 12)).get

  println {
    Metricas.tempoExecucaoPorArquivoEhQuantidadeLinha {
      arquivo =>
        val acoes = ListBuffer[Option[Acao]]()
        val linhas = Source.fromFile(arquivo, "UTF-8").getLines.toList
        for (i <- linhas.indices.toList) {
          val line = linhas(i)
          val acao = new AcaoFileLineValidator(line, ugArquivo, i + 1, dataCompetenciaArquivo).getEntidade(acao => acoes.filter(_.isDefined).exists(_.get.equalsTo(acao.get)))
          if(acao.isDefined) acoes += acao
        }
    }
  }

}
