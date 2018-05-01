import java.nio.charset.Charset

import sagres.model.{ControleArquivo, ErroImportacao, ImportacaoException}
import sagres.utils.DateUtils
import tcc.Metricas
import tcc.validador._

import scala.io.{Codec, Source}

object RotinaFuncionalTeste extends App {

  val nomeArquivo = "201095012018Acao.txt"
  val ugArquivo = nomeArquivo.substring(0, 6)
  implicit val codec: Codec = Codec(Charset.forName("UTF-8"))
  val arquivo = Source.fromResource("201095012018Acao.txt")
  val listaArquivo = arquivo.getLines.toList
  implicit val controle: ControleArquivo = ControleArquivo(None, 10, 2018, "Acao", 287, ativo = true, 0, 2, None)
  implicit val erros: ImportacaoException = ImportacaoException("Falha na importação", erroImportacaoBase = ErroImportacao(None, "201095", 2018, 1, None, 1, None, None, None, None, None))
  val dataCompetenciaArquivo = DateUtils.stringToSqlDate(nomeArquivo.substring(6, 12)).get

  def adicionarErroAhExcecao(erro: TipoErro): Unit = {
    erros.adicionarErro(erro.codigoArquivo, erro.numeroLinha, erro.conteudoLinha, erro.msg, erro.tipoErroImportacaoEnum)
  }

  println {
    Metricas.tempoExecucaoPorArquivoEhQuantidadeLinha {
      arquivo => Validador.validarAcaoFromFile(arquivo, dataCompetenciaArquivo, ugArquivo, controle).foreach {
        case ResultadosErro(erros) =>
          println(erros.foldLeft("")((acumulador, entidade) => s"$acumulador\n$entidade" ))
          erros.foreach(adicionarErroAhExcecao)
          Seq.empty
        case ResultadosAviso(erros, entidades) =>
          erros.foreach(adicionarErroAhExcecao)
          entidades
        case ResultadosSucesso(entidades) =>
          entidades
      }
    }
  }

}
