package tcc.validador
abstract class Validador[A, B]{

  abstract protected def gerarEntidadeArquivo(linha: String): B

  abstract protected val etapas: Seq[Seq[String => Option[TipoErro]]]

  abstract protected def gerarEntidade(entidadeArquivo: B): Option[A]

  protected def processarEtapa(entidadeArquivo: B, etapa: Seq[String => Option[TipoErro]]): Seq[TipoErro] = {
    etapa.flatMap(regra => regra(entidadeArquivo))
  }

  protected def processarEtapas(entidadeArquivo: B, etapas: Seq[Seq[String => Option[TipoErro]]], errosAnteriores: Seq[TipoErro] = Seq()): ResultadoValidacao[A] = {
    etapas match {
      case Nil => ResultadoValidacao(Option(errosAnteriores), gerarEntidade(entidadeArquivo))
      case etapa :: proximas =>
        val erros = processarEtapa(entidadeArquivo, etapa)
        if (TipoErro.existeTipoErro(erros))
          ResultadoValidacao(Some(errosAnteriores ++: erros), None)
        else
          processarEtapas(entidadeArquivo, proximas, errosAnteriores ++: erros)
    }
  }

  final def validar(linha: String): ResultadoValidacao[A] ={
    val entidadeArquivo = gerarEntidadeArquivo(linha)
    processarEtapas(entidadeArquivo, etapas)
  }
}

sealed abstract class TipoErro

object TipoErro {

  def existeTipoErro(lista: Seq[TipoErro]): Boolean = lista match {
    case Nil => false
    case head :: tail => head match {
        case Erro(_) => true
        case _ => existeTipoErro(tail)
    }
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
      case (Some(erros), Some(entidade)) => verificarCasoErroEntidade(erros, entidade)
      case (None, Some(entidade)) => ResultadoSucesso(entidade)
    }
  }

  private def verificarCasoErroEntidade[A](erros: Seq[TipoErro], entidade: A): ResultadoValidacao[A] = {
    if(TipoErro.existeTipoErro(erros)) ResultadoErro(erros)
    else erros match {
      case Nil => ResultadoSucesso(entidade)
      case _ => ResultadoAviso(erros, entidade)
    }
  }
}
