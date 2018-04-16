package tcc.validador

import tcc.validador.validadores.entidades.EntidadeArquivo
import scala.util.Try

protected[validador] case class SubEtapa(regras: Seq[Regra], processador: (SubEtapa, EntidadeArquivo, MetaDados) => Try[Seq[TipoErro]] = Processadores.processarSubEtapa)