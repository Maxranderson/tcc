package tcc

import sagres.model.ControleArquivo
import tcc.validador.ValidadorAcao
import sagres.validator.AcaoFileLineValidator

object Teste extends App{
  val linha = ""
  val ugArquivo = ""
  val numeroLinha = 0
  implicit val controle = ControleArquivo(None, 10, 2018, "Acao", 120, true, 0, 2, None)
  val resultado = ValidadorAcao.validar(linha)
  val resultado2 = new AcaoFileLineValidator()
}
