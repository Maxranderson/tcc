package tcc

import java.nio.charset.Charset

import sagres.model._
import sagres.utils.DateUtils
import sagres.validator.AcaoFileLineValidator
import tcc.validador._

import scala.collection.mutable.ListBuffer
import scala.io.{Codec, Source}

object Teste extends App {

  val nomeArquivo = "201095012018Acao.txt"
  val ugArquivo = nomeArquivo.substring(0, 6)
  implicit val codec: Codec = Codec(Charset.forName("UTF-8"))
  val linhas = Source.fromResource("201095012018Acao.txt").getLines().toList
  implicit val controle: ControleArquivo = ControleArquivo(None, 10, 2018, "Acao", 287, ativo = true, 0, 2, None)
  implicit val erros: ImportacaoException = ImportacaoException("Falha na importação", erroImportacaoBase = ErroImportacao(None, "201095", 2018, 1, None, 1, None, None, None, None, None))

  val dataCompetenciaArquivo = DateUtils.stringToSqlDate(nomeArquivo.substring(6, 12)).orNull
  val resultadoOO = Metricas.tempoExecucaoComResultado {
    val acoes = ListBuffer[Option[Acao]]()

    if(dataCompetenciaArquivo == null){

      erros.adicionarErro(controle.codigoArquivo, 0, "", "Data da competência no arquivo está inválida", TipoErroImportacaoEnum.ERROR)

    }else{

      for (i <- linhas.indices.toList) {
        val line = linhas(i)


        //TODO implementar o acaoFileLineValidator
        val acao = new AcaoFileLineValidator(line, ugArquivo, i + 1, dataCompetenciaArquivo).getEntidade(acao => acoes.filter(_.isDefined).exists(_.get.equalsTo(acao.get)))
        if(acao.isDefined) acoes += acao
      }

    }
  }

  val resultFuncional = Metricas.tempoExecucaoComResultado {

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
