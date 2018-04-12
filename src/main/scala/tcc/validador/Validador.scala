package tcc.validador

import java.sql.Date

import sagres.model.{ControleArquivo, TipoErroImportacaoEnum}
import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum

import scala.util.{Failure, Success, Try}

abstract class Validador[A] {

  protected def gerarEntidadeArquivo(linha: String, metaDados: MetaDadosValidacao): entidadeArquivo

  protected def gerarEtapas(anoCompetencia: Int): Seq[Etapa]

  protected def gerarEntidade(entidadeArquivo: entidadeArquivo, metaDadosValidacao: MetaDadosValidacao): Option[A]

  protected def processarEtapas(entidadeArquivo: entidadeArquivo, dadosValidacao: MetaDadosValidacao, etapas: Seq[Etapa], errosAnteriores: Seq[TipoErro] = Seq[TipoErro]()): ResultadoValidacao[A] = {
    etapas match {
      case Nil => ResultadoValidacao(Option(errosAnteriores), gerarEntidade(entidadeArquivo, dadosValidacao))
      case etapa :: proximas =>
        val tryErros = etapa.processador(entidadeArquivo, dadosValidacao, etapa.regras)
        if (tryErros.isFailure || tryErros.map(erros => TipoErro.existeTipoErro(erros)).getOrElse(true))
          ResultadoValidacao(Some(errosAnteriores ++: erros), None)
        else
          processarEtapas(entidadeArquivo, dadosValidacao,proximas, errosAnteriores ++: erros)
    }
  }

  protected def processoValidacao(linha: String, numeroLinha: Int, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo): ResultadoValidacao[A] = {
    val metaDados = MetaDadosValidacao(linha, numeroLinha, dataCompetencia, unidadeGestoraArquivo, controleArquivo)
    processarEtapas(gerarEntidadeArquivo(linha, metaDados), metaDados,Etapa(List(linhaDiferenteDoControleArquivo)) +: gerarEtapas(controleArquivo.ano), Seq[TipoErro]())
  }

  final def validar: (String, Int, Date, String, ControleArquivo) => ResultadoValidacao[A] = processoValidacao
}

final case class MetaDadosValidacao(conteudoLinha: String, numeroLinha: Int, dataCompetencia: Date, unidadeGestoraArquivo: String, controleArquivo: ControleArquivo)

trait entidadeArquivo

case class Etapa(regras: Seq[Regra], processador: (entidadeArquivo, MetaDadosValidacao, Seq[Regra]) => Try[Seq[TipoErro]] = Utils.processarEtapa)

case class Regra(validar: (entidadeArquivo, MetaDadosValidacao) => Try[Option[TipoErro]])

object Regra {

  def apply[T](tipo: TipoErroImportacaoEnum, mensagem: String)(decisao: (T, MetaDadosValidacao) => Try[Boolean]): Regra = new Regra(Utils.fazerRegra(tipo, mensagem)(decisao))
}

package object Utils {
  def castEntidadeTrait[T](entidade: entidadeArquivo)(code: (T) => Try[Option[TipoErro]]): Try[Option[TipoErro]] = {
    entidade match {
      case e: entidadeArquivo with T => code(e)
      case _ => Failure(new Exception("Não foi possivel fazer o cast da entidade"))
    }
  }

  def fazerRegra[T](tipo: TipoErroImportacaoEnum, mensagem: String)(decisao: (T, MetaDadosValidacao) => Try[Boolean])(entidadeArquivo: entidadeArquivo, dadosValidacao: MetaDadosValidacao): Try[Option[TipoErro]] = {
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

  def processarEtapa(entidadeArquivo: entidadeArquivo, dadosValidacao: MetaDadosValidacao, etapa: Seq[Regra]): Try[Seq[TipoErro]] = {
    def loop(etapa: Seq[Regra], acumulador: Seq[TipoErro] = Seq()): Try[Seq[TipoErro]] = {
      etapa match {
        case Nil => Success(acumulador)
        case head :: tail =>
          head.validar(entidadeArquivo, dadosValidacao) match {
            case Success(tipoErro) =>
              tipoErro match {
                case Some(valor) => loop(tail, valor +: acumulador)
                case None => loop(tail, acumulador)
              }
            case Failure(e) => Failure(e)
          }
      }
    }
    loop(etapa)
  }

//  def processarEtapaParalelo(entidadeArquivo: entidadeArquivo, dadosValidacao: MetaDadosValidacao, etapa: Seq[Regra]): Try[Seq[TipoErro]] = {
//    etapa.par.flatMap(regra => regra.validar(entidadeArquivo, dadosValidacao)).toVector
//  }
}