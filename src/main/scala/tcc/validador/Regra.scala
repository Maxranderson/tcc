package tcc.validador

import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum
import tcc.validador.validadores.entidades.EntidadeArquivo

import scala.util.Try

protected[validador] case class Regra(validar: (EntidadeArquivo, MetaDados) => Try[Option[TipoErro]])

protected[validador] object Regra {
  def apply[T](tipo: TipoErroImportacaoEnum, mensagem: String)(decisao: (T, MetaDados) => Try[Boolean]): Regra = new Regra(fazerRegra(tipo, mensagem)(decisao))

  def fazerRegra[T <: EntidadeArquivo](tipo: TipoErroImportacaoEnum, mensagem: String)(decisao: (T, MetaDados) => Try[Boolean])(entidadeArquivo: EntidadeArquivo, dadosValidacao: MetaDados): Try[Option[TipoErro]] = {
    decisao(entidadeArquivo.asInstanceOf[T], dadosValidacao).map {
      decisao => {
        if(decisao){
          Option(
            TipoErro(
              dadosValidacao.controleArquivo.codigoArquivo,
              dadosValidacao.numeroLinha,
              dadosValidacao.conteudoLinha,
              mensagem,
              tipo
            )
          )
        }else None
      }
    }
  }
}