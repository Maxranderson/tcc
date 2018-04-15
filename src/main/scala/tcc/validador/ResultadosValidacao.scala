package tcc.validador

protected[validador] sealed trait ResultadosValidacao[A]

sealed case class ResultadosErro[A](erros: Seq[TipoErro]) extends ResultadosValidacao[A]

sealed case class ResultadosAviso[A](erros: Seq[TipoErro], entidade: Seq[A]) extends ResultadosValidacao[A]

sealed case class ResultadosSucesso[A](entidade: Seq[A]) extends ResultadosValidacao[A]

protected[validador] object ResultadosValidacao {
  def apply[A](erros: Seq[TipoErro], entidadesOption: Option[Seq[A]]): ResultadosValidacao[A] = {
    (erros, entidadesOption) match {
      case (erros, None) => ResultadosErro(erros)
      case (erros, Some(entidades)) if TipoErro.existeTipoErro(erros) => ResultadosErro(erros)
      case (Nil, Some(entidades)) => ResultadosSucesso(entidades)
      case (erros, Some(entidades)) => ResultadosAviso(erros, entidades)
    }
  }
}