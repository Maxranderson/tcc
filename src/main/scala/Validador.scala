abstract class Validador[A]{

  abstract val entidadeArquivo: Validador[A]

  abstract val etapas: Seq[Seq[String => Option[ErrorType]]]

  abstract def gerarEntidade(entidadeArquivo: Validador[A]): Option[A]
  
  def processarEtapa(etapa: Seq[String => Option[ErrorType]]): Seq[ErrorType] = {
    etapa.flatMap(regra => regra(entidadeArquivo))
  }

  def processarEtapas(etapas: Seq[Seq[String => Option[ErrorType]]], errosAnteriores: Seq[ErrorType] = Seq()): ValidationResult[A] = {
    etapas match {
      case Nil => errosAnteriores match {
        case Nil => ValidationResult(None, gerarEntidade(entidadeArquivo))
        case _ => ValidationResult(Option(errosAnteriores), gerarEntidade(entidadeArquivo))
      }
      case etapa :: proximas => {
        val erros = processarEtapa(etapa)
        if (ErrorType.existeTipoError(erros))
          ValidationResult(Some(errosAnteriores ++: erros), None)
        else
          processarEtapas(proximas, errosAnteriores ++: erros)
      }
    }
  }

  final def validar(linha: String): ValidationResult[A] = processarEtapas(etapas)
}

sealed abstract class ErrorType

object ErrorType {

  def existeTipoError(lista: Seq[ErrorType]): Boolean = lista match {
      case head :: tail => head match {
        case Error(msg) => true
        case _ => false
      }
      case Nil => false
    }

}

case class Error(msg: String) extends ErrorType

case class Warning(msg: String) extends ErrorType

sealed abstract class ValidationResult[A]

case class ErrorResult[A](erros: Seq[ErrorType]) extends ValidationResult[A]

case class WarningResult[A](erros: Seq[ErrorType], entidade: A) extends ValidationResult[A]

case class SuccessResult[A](entidade: A) extends ValidationResult[A]

object ValidationResult {
  def apply[A](errosOption: Option[Seq[ErrorType]], entidadeOption: Option[A]): ValidationResult[A] = {
    (errosOption, entidadeOption) match {
      case (Some(erros), None) => ErrorResult(erros)
      case (Some(erros), Some(entidade)) => WarningResult(erros, entidade)
      case (None, Some(entidade)) => SuccessResult(entidade)
    }
  }
}
