package tcc.validador.validadores.entidades

import tcc.validador.MetaDados

protected[validador] trait EntidadeArquivo

protected[validador] trait Conversor[A] {

  def entidadeArquivoParaEntidade(entidadeArquivo: EntidadeArquivo, metaDados: MetaDados): A

  def entidadeArquivoIgualAhEntidade(entidadeArquivo: EntidadeArquivo, entidade: A): Boolean

  def fromFileLine(metaDados: MetaDados): EntidadeArquivo

  def fromJson(): EntidadeArquivo
}
