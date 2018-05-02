package tcc.validador

import java.sql.Date

import sagres.model.{ControleArquivo, ErroImportacao}

protected[validador] final case class MetaDados(conteudoLinha: String, numeroLinha: Int, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo, erroBase: ErroImportacao)