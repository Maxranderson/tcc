import java.io.File
import java.nio.charset.Charset

import sagres.model.{Acao, ControleArquivo, ErroImportacao, ImportacaoException}
import sagres.utils.DateUtils
import tcc.Metricas
import tcc.validador._

import scala.collection.mutable.ListBuffer
import scala.io.{Codec, Source}
import scala.util.Try

object RotinaFuncionalTeste extends App {

  val nomeArquivo = "201095012018Acao.txt"
  val ugArquivo = nomeArquivo.substring(0, 6)
  implicit val codec: Codec = Codec(Charset.forName("UTF-8"))
  val arquivo = Source.fromResource(nomeArquivo)
  val listaArquivo = arquivo.getLines.toList
  implicit val controle: ControleArquivo = ControleArquivo(None, 10, 2018, "Acao", 287, ativo = true, 0, 2, None)
  implicit val erros: ImportacaoException = ImportacaoException("Falha na importação", erroImportacaoBase = ErroImportacao(None, "201095", 2018, 1, None, 1, None, None, None, None, None))
  val dataCompetenciaArquivo = DateUtils.stringToSqlDate(nomeArquivo.substring(6, 12)).get

  def adicionarErroAhExcecao(erro: TipoErro): Unit = {
    erros.adicionarErro(erro.codigoArquivo, erro.numeroLinha, erro.conteudoLinha, erro.msg, erro.tipoErroImportacaoEnum)
  }

  def adicionarErrosNaExcecao(erros2: Seq[TipoErro]): Unit ={
    erros.adicionarErros(erros2)
  }

  println {
    Metricas.tempoExecucaoPorArquivoEhQuantidadeLinha {
      arquivo =>
        Validador.validarAcaoFromFile(arquivo, dataCompetenciaArquivo, ugArquivo, controle, erros).map {
        case ResultadosErro(erros) =>
          adicionarErrosNaExcecao(erros)
          Seq.empty
        case ResultadosAviso(erros, entidades) =>
          adicionarErrosNaExcecao(erros)
          entidades
        case ResultadosSucesso(entidades) =>
          entidades
      }
    }
  }
}
