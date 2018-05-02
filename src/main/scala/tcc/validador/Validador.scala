package tcc.validador

import java.io.File
import java.sql.Date

import sagres.model.{Acao, ControleArquivo, ErroImportacao}
import tcc.validador.validadores.Validadores

import scala.util.Try

object Validador extends Rotina{

  def validarAcaoFromFile(file: File,
                          dataCompetencia: Date,
                          unidadeGestoraArquivo: String,
                          controleArquivo: ControleArquivo,
                          erroBase: ErroImportacao): Try[ResultadosValidacao[Acao]] = {
    validarFromFile(file, dataCompetencia, unidadeGestoraArquivo, controleArquivo, erroBase, Validadores.validarAcao)
  }

  def validarAcaoFromFileParalelo(file: File,
                          dataCompetencia: Date,
                          unidadeGestoraArquivo: String,
                          controleArquivo: ControleArquivo,
                          erroBase: ErroImportacao): Try[ResultadosValidacao[Acao]] = {
    validarFromFileParalelo(file, dataCompetencia, unidadeGestoraArquivo, controleArquivo, erroBase, Validadores.validarAcao)
  }

}

