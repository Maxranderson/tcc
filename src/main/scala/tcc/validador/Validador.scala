package tcc.validador

import java.sql.Date

import sagres.model.{ControleArquivo, TipoErroImportacaoEnum}
import sagres.model.TipoErroImportacaoEnum.TipoErroImportacaoEnum
import tcc.validador.entidades.EntidadeArquivo

import scala.util.{Failure, Success, Try}

abstract class Validador[A] {

  protected def gerarEntidadeArquivo(linha: String, metaDados: MetaDadosValidacao): EntidadeArquivo

  protected def gerarEtapas(anoCompetencia: Int): Seq[Etapa]

  protected def gerarEntidade(entidadeArquivo: EntidadeArquivo, metaDadosValidacao: MetaDadosValidacao): Option[A]

  protected def processarEtapas(entidadeArquivo: EntidadeArquivo, dadosValidacao: MetaDadosValidacao, etapas: Seq[Etapa], errosAnteriores: Seq[TipoErro] = Seq[TipoErro]()): ResultadoValidacao[A] = {
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

package object Utils {
  def castEntidadeTrait[T](entidade: EntidadeArquivo)(code: (T) => Try[Option[TipoErro]]): Try[Option[TipoErro]] = {
    entidade match {
      case e: EntidadeArquivo with T => code(e)
      case _ => Failure(new Exception("Não foi possivel fazer o cast da entidade"))
    }
  }

//  def processarEtapaParalelo(entidadeArquivo: entidadeArquivo, dadosValidacao: MetaDadosValidacao, etapa: Seq[Regra]): Try[Seq[TipoErro]] = {
//    etapa.par.flatMap(regra => regra.validar(entidadeArquivo, dadosValidacao)).toVector
//  }
}