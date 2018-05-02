package tcc.validador

import java.io.File
import java.sql.Date

import sagres.model.{ControleArquivo, ErroImportacao}
import tcc.validador.validadores.entidades.Conversor

import scala.io.Source
import scala.util.Try

protected[validador] trait Rotina {
  def validarFromFile[A](file: File, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo, erroBase: ErroImportacao, gerarTuplaProcessadorEtapas: (MetaDados) => Try[(Conversor[A], Seq[Etapa])]): Try[ResultadosValidacao[A]] = {
    Processadores.processarLinhasSequencial(Source.fromFile(file, "UTF-8").getLines().toList.zipWithIndex, MetaDados("", 0, dataCompetencia, unidadeGestoraArquivo, controleArquivo, erroBase), gerarTuplaProcessadorEtapas)
  }

  def validarFromFileParalelo[A](file: File, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo, erroBase: ErroImportacao, gerarTuplaProcessadorEtapas: (MetaDados) => Try[(Conversor[A], Seq[Etapa])]): Try[ResultadosValidacao[A]] = {
    Processadores.processarLinhasParalelo(Source.fromFile(file, "UTF-8").getLines().toVector.zipWithIndex, MetaDados("", 0, dataCompetencia, unidadeGestoraArquivo, controleArquivo, erroBase), gerarTuplaProcessadorEtapas)
  }
}