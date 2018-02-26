package tcc.validador
abstract class Validador[A, B]{

  abstract val entidadeArquivo: B

  abstract val etapas: Seq[Seq[String => Option[TipoErro]]]

  abstract def gerarEntidade(entidadeArquivo: B): Option[A]
  
  def processarEtapa(etapa: Seq[String => Option[TipoErro]]): Seq[TipoErro] = {
    etapa.flatMap(regra => regra(entidadeArquivo))
  }

  def processarEtapas(etapas: Seq[Seq[String => Option[TipoErro]]], errosAnteriores: Seq[TipoErro] = Seq()): ResultadoValidacao[A] = {
    etapas match {
      case Nil => errosAnteriores match {
        case Nil => ResultadoValidacao(None, gerarEntidade(entidadeArquivo))
        case _ => ResultadoValidacao(Option(errosAnteriores), gerarEntidade(entidadeArquivo))
      }
      case etapa :: proximas => {
        val erros = processarEtapa(etapa)
        if (TipoErro.existeTipoErro(erros))
          ResultadoValidacao(Some(errosAnteriores ++: erros), None)
        else
          processarEtapas(proximas, errosAnteriores ++: erros)
      }
    }
  }

  final def validar(linha: String): ResultadoValidacao[A] = processarEtapas(etapas)
}

sealed abstract class TipoErro

object TipoErro {

  def existeTipoErro(lista: Seq[TipoErro]): Boolean = lista match {
      case head :: tail => head match {
        case Erro(msg) => true
        case _ => false
      }
      case Nil => false
    }

}

case class Erro(msg: String) extends TipoErro

case class Aviso(msg: String) extends TipoErro

sealed abstract class ResultadoValidacao[A]

case class ResultadoErro[A](erros: Seq[TipoErro]) extends ResultadoValidacao[A]

case class ResultadoAviso[A](erros: Seq[TipoErro], entidade: A) extends ResultadoValidacao[A]

case class ResultadoSucesso[A](entidade: A) extends ResultadoValidacao[A]

object ResultadoValidacao {
  def apply[A](errosOption: Option[Seq[TipoErro]], entidadeOption: Option[A]): ResultadoValidacao[A] = {
    (errosOption, entidadeOption) match {
      case (Some(erros), None) => ResultadoErro(erros)
      case (Some(erros), Some(entidade)) => ResultadoAviso(erros, entidade)
      case (None, Some(entidade)) => ResultadoSucesso(entidade)
    }
  }
}
