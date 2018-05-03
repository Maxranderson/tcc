package tcc.validador

import sagres.model.ErroImportacao
import tcc.validador.validadores.entidades.EntidadeArquivo

import scala.util.Try

protected[validador] case class SubEtapa(regras: Seq[Regra], processador: (SubEtapa, EntidadeArquivo, MetaDados) => Try[(Boolean, Seq[ErroImportacao])] = Processadores.processarSubEtapa)