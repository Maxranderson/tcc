package tcc.validador

import sagres.model.ErroImportacao
import tcc.validador.validadores.entidades.EntidadeArquivo

import scala.util.Try

protected[validador] case class Etapa(subEtapas: Seq[SubEtapa], processador: (Etapa, EntidadeArquivo, MetaDados) => Try[(Boolean, Seq[ErroImportacao])] = Processadores.processarEtapaSequencial)