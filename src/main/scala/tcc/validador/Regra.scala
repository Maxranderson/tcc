package tcc.validador

import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum
import tcc.validador.entidades.EntidadeArquivo

import scala.util.{Failure, Success, Try}

case class Regra(validar: (EntidadeArquivo, MetaDadosValidacao) => Try[Option[TipoErro]])

object Regra {
  def apply[T](tipo: TipoErroImportacaoEnum, mensagem: String)(decisao: (T, MetaDadosValidacao) => Try[Boolean]): Regra = new Regra(fazerRegra(tipo, mensagem)(decisao))

  def fazerRegra[T](tipo: TipoErroImportacaoEnum, mensagem: String)(decisao: (T, MetaDadosValidacao) => Try[Boolean])(entidadeArquivo: EntidadeArquivo, dadosValidacao: MetaDadosValidacao): Try[Option[TipoErro]] = {
    Utils.castEntidadeTrait[T](entidadeArquivo) { entidadeArquivo =>
      tratarExcecaoRegra(decisao(entidadeArquivo, dadosValidacao)){
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

  def tratarExcecaoRegra(tryDecisao: Try[Boolean])(code: (Boolean) => Option[TipoErro]): Try[Option[TipoErro]] = {
    tryDecisao match {
      case Success(valor) => Success(code(valor))
      case Failure(e) => Failure(e)
    }
  }
}