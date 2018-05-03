package tcc.validador.validadores.entidades

import java.io.File

import sagres.model.Acao
import tcc.validador.MetaDados

protected[validadores] case class ArquivoAcao(
                        codigoUnidadeGestora: String,
                        codigoAcao: String,
                        denominacaoAcao: String,
                        tipoAcao: String,
                        descricaoMeta: String,
                        unidadeMedida: String
                      ) extends EntidadeArquivo with UnidadeGestoraRel {

}

protected[validadores] object ArquivoAcao extends Conversor[Acao] {
  def entidadeArquivoParaEntidade(entidadeArquivo: EntidadeArquivo, metaDados: MetaDados) = {
    entidadeArquivo match {
      case entidade: ArquivoAcao =>
        Acao(
          None,
          entidade.codigoUnidadeGestora,
          metaDados.dataCompetencia.toLocalDate.getYear,
          entidade.codigoAcao,
          Option(entidade.denominacaoAcao),
          Option(entidade.tipoAcao),
          Option(entidade.unidadeMedida),
          Option(entidade.descricaoMeta),
          1
        )
      case _ => throw new Exception("Não foi possível converter a entidade.")
    }

  }

  def fromFileLine(metaDados: MetaDados): EntidadeArquivo = {
    val getCampo: String => String =  metaDados.controleArquivo.getCampo(metaDados.conteudoLinha, _)
    ArquivoAcao(
      getCampo("codigoUnidadeGestora"),
      getCampo("codigoAcao"),
      getCampo("denominacaoAcao"),
      getCampo("tipoAcao"),
      getCampo("descricaoMeta"),
      getCampo("unidadeMedida")
    )
  }

  def fromJson(): EntidadeArquivo = {
    ArquivoAcao("", "", "", "", "", "")
  }

  def entidadeArquivoIgualAhEntidade(entidadeArquivo: EntidadeArquivo, entidade: Acao): Boolean = {
    entidade.unidadeGestora == entidadeArquivo.asInstanceOf[ArquivoAcao].codigoUnidadeGestora &&
    entidade.codigo == entidadeArquivo.asInstanceOf[ArquivoAcao].codigoAcao
  }

  def entidadeIguaAhEntidade(e1: Acao, e2: Acao): Boolean = e1.equalsTo(e2)
}
