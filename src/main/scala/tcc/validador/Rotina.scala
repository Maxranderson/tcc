package tcc.validador

import java.io.File
import java.sql.Date

import sagres.model.ControleArquivo
import tcc.validador.validadores.entidades.Conversor

import scala.io.Source
import scala.util.Try

protected[validador] trait Rotina {
  def validarFromFile[A](file: File, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo, gerarTuplaProcessadorEtapas: (MetaDados) => Try[(Conversor[A], Seq[Etapa])]): Try[ResultadosValidacao[A]] = {
    Processadores.processarLinhasSequencial(Source.fromFile(file, "UTF-8").getLines().toList.zipWithIndex, MetaDados("", 0, dataCompetencia, unidadeGestoraArquivo, controleArquivo), gerarTuplaProcessadorEtapas)
  }
}