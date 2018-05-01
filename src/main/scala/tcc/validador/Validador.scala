package tcc.validador

import java.io.File
import java.sql.Date

import sagres.model.{Acao, ControleArquivo}
import tcc.validador.validadores.Validadores
import scala.util.{Try}

object Validador extends Rotina{

  def validarAcaoFromFile(file: File,
                          dataCompetencia: Date,
                          unidadeGestoraArquivo: String,
                          controleArquivo: ControleArquivo): Try[ResultadosValidacao[Acao]] = {
    validarFromFile(file, dataCompetencia, unidadeGestoraArquivo, controleArquivo, Validadores.validarAcao)
  }

  def validarAcaoFromFileParalelo(file: File,
                          dataCompetencia: Date,
                          unidadeGestoraArquivo: String,
                          controleArquivo: ControleArquivo): Try[ResultadosValidacao[Acao]] = {
    validarFromFileParalelo(file, dataCompetencia, unidadeGestoraArquivo, controleArquivo, Validadores.validarAcao)
  }

}

