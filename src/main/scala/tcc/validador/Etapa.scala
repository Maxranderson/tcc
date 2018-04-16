package tcc.validador

import tcc.validador.validadores.entidades.EntidadeArquivo

import scala.util.Try

protected[validador] case class Etapa(subEtapas: Seq[SubEtapa], processador: (Etapa, EntidadeArquivo, MetaDados) => Try[Seq[TipoErro]] = Processadores.processarEtapaSequencial)