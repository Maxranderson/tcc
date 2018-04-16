package tcc.validador

import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum
import tcc.Metricas
import tcc.validador.validadores.entidades.EntidadeArquivo

import scala.util.{Failure, Success, Try}

protected[validador] case class Regra(validar: (EntidadeArquivo, MetaDados) => Try[Option[TipoErro]])

protected[validador] object Regra {
  def apply[T](tipo: TipoErroImportacaoEnum, mensagem: String)(decisao: (T, MetaDados) => Try[Boolean]): Regra = new Regra(fazerRegra(tipo, mensagem)(decisao))

  def castEntidadeTrait[T](entidade: EntidadeArquivo)(code: (T) => Try[Option[TipoErro]]): Try[Option[TipoErro]] = {
    entidade match {
      case e: T => code(e)
      case _ => Failure(new Exception("Não foi possivel fazer o cast da entidade"))
    }
  }

  def fazerRegra[T](tipo: TipoErroImportacaoEnum, mensagem: String)(decisao: (T, MetaDados) => Try[Boolean])(entidadeArquivo: EntidadeArquivo, dadosValidacao: MetaDados): Try[Option[TipoErro]] = {
    castEntidadeTrait[T](entidadeArquivo) { entidadeArquivo =>
      decisao(entidadeArquivo, dadosValidacao).map {
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
}