package sagres.validator

trait FileLineValidator[T] {
    protected val line : String
    protected val lineNumber: Int
    protected val ugArquivo: String

    /**
      * Recebe uma função para verificar se a entidade não está duplicada no arquivo. Valida a linha do arquivo.
      *
      * @return
      *         Option[T] caso a validação não encontre erros.
      */
    def getEntidade( entidade: Option[T] => Boolean ) : Option[T]
}