package tcc

import sagres.model.{Acao, ControleArquivo}
import sagres.utils.DateUtils
import tcc.validador.ValidadorAcao
import sagres.validator.AcaoFileLineValidator

import scala.io.Source

object Teste extends App{
  val nomeArquivo = "201095012018Acao.txt"
  val ugArquivo = nomeArquivo.substring(0, 6)
  val linhas = Source.fromResource("201095012018Acao.txt").getLines().toList
  implicit val controle: ControleArquivo = ControleArquivo(None, 10, 2018, "Acao", 120, ativo = true, 0, 2, None)
  DateUtils.stringToSqlDate("20180101") match {
    case Some(dataCompetencia) =>
      linhas.zipWithIndex.foldLeft(List[Acao]())((lista, tupla) => {
        ValidadorAcao.validar(tupla._1, tupla._2 + 1, dataCompetencia, ugArquivo, controle).

      })
      val resultado2 = new AcaoFileLineValidator(linha, ugArquivo, numeroLinha, dataCompetencia)
    case None => ???
  }


}
