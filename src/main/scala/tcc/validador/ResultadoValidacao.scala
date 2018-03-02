package tcc.validador

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