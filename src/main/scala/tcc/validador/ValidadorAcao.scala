package tcc.validador

import sagres.model.Acao

object ValidadorAcao extends Validador[Acao, ArquivoAcao]{
  override protected def gerarEntidadeArquivo(linha: String): ArquivoAcao = ArquivoAcao(linha.substring(0,6))

  override protected val etapas: Seq[Seq[String => Option[TipoErro]]] = ???

  override protected def gerarEntidade(entidadeArquivo: ArquivoAcao): Option[Acao] = ???
}

case class ArquivoAcao(ug: String)
