package tcc

import sagres.model.ControleArquivo
import sagres.utils.DateUtils
import tcc.validador.ValidadorAcao
import sagres.validator.AcaoFileLineValidator

object Teste extends App{
  val linha = ""
  val ugArquivo = ""
  val numeroLinha = 0
  implicit val controle = ControleArquivo(None, 10, 2018, "Acao", 120, true, 0, 2, None)
  DateUtils.stringToSqlDate("20180101") match {
    case Some(dataCompetencia) =>
      val resultado = ValidadorAcao.validar(linha, numeroLinha, dataCompetencia, ugArquivo, controle)
      val resultado2 = new AcaoFileLineValidator(linha, ugArquivo, numeroLinha, dataCompetencia)
    case None => ???
  }


}
