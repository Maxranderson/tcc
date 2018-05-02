package tcc.validador

import sagres.model.ErroImportacao

sealed trait ResultadosValidacao[A]

sealed case class ResultadosErro[A](erros: Seq[ErroImportacao]) extends ResultadosValidacao[A]

sealed case class ResultadosAviso[A](erros: Seq[ErroImportacao], entidade: Seq[A]) extends ResultadosValidacao[A]

sealed case class ResultadosSucesso[A](entidade: Seq[A]) extends ResultadosValidacao[A]

protected[validador] object ResultadosValidacao {
  def apply[A](booleanErros: (Boolean, Seq[ErroImportacao]), entidadesOption: Option[Seq[A]]): ResultadosValidacao[A] = {
    val (comErros, erros) = booleanErros
    (erros, entidadesOption) match {
      case (erros, None) => ResultadosErro(erros)
      case (erros, Some(entidades)) if comErros => ResultadosErro(erros)
      case (Nil, Some(entidades)) => ResultadosSucesso(entidades)
      case (erros, Some(entidades)) => ResultadosAviso(erros, entidades)
    }
  }
}