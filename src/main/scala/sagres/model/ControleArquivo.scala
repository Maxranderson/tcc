package sagres.model


case class ControleArquivo(id: Option[Long],
                           codigoArquivo: Int,
                           ano: Int,
                           nomeArquivo: String,
                           tamanhoLinha: Int,
                           ativo: Boolean,
                           tipoSistema: Int,
                           ordem: Int,
                           layout: Option[String],
                           var cancelled: Option[Long] = None
                          ) {

  def getCampo(linha: String, campo: String): String = {
    nomeArquivo match {
      case "Acao" => acaoLayout(linha, campo)
      case "Programa" => programaLayout(linha, campo)
      case "UnidadeOrcamentaria" => unidadeOrcamentariaLayout(linha, campo)
      case "Dotacao" => dotacaoLayout(linha, campo)
    }
  }

  def getTamanho(campo: String): Int = {
    nomeArquivo match {
      case "Acao" => campoAcaoLayout(campo)._2
      case "Programa" => campoProgramaLayout(campo)._2
      case "UnidadeOrcamentaria" => campoUOLayout(campo)._2
      case "Dotacao" => campoDotacaoLayout(campo)._2
    }
  }

  def campoAcaoLayout(campo: String): (Int, Int) = {
    val codigoUnidadeGestora = (0, 6)
    val codigoAcao = (6, 4)
    val denominacaoAcao = (10, 70)
    val tipoAcao = (80, 1)
    val descricaoMeta = (81, 150)
    val unidadeMedida = (231, 50)

    campo match {
      case "codigoUnidadeGestora" => codigoUnidadeGestora
      case "codigoAcao" => codigoAcao
      case "denominacaoAcao" => denominacaoAcao
      case "tipoAcao" => tipoAcao
      case "descricaoMeta" => descricaoMeta
      case "unidadeMedida" => unidadeMedida
    }
  }

  def acaoLayout(linha: String, campo: String): String = substringLinha(linha, campoAcaoLayout(campo))

  def campoProgramaLayout(campo: String): (Int, Int) = {
    val codigoUnidadeGestora = (0,6)
    val codigoPrograma = (6,4)
    val denominacaoPrograma = (10,70)
    val descricaoObjetivoPrograma = (80,150)
    val tipoObjetivoMilenio = (230,2)

    campo match {
      case "codigoUnidadeGestora" => codigoUnidadeGestora
      case "codigoPrograma" => codigoPrograma
      case "denominacaoPrograma" => denominacaoPrograma
      case "descricaoObjetivoPrograma" => descricaoObjetivoPrograma
      case "tipoObjetivoMilenio" => tipoObjetivoMilenio
    }
  }

  def programaLayout(linha: String, campo: String): String = substringLinha(linha, campoProgramaLayout(campo))

  def campoUOLayout(campo: String): (Int, Int) = {
    val codigoUnidadeGestora = (0, 6)
    val codigoUnidadeOrcamentaria = (6, 5)
    val denominacaoUnidadeOrcamentaria = (11, 50)
    val nomeSecretarioMunicipal = (61, 60)
    val cpfSecretarioMunicipal = (121, 11)
    val atoAdministrativo = (132, 1)
    val tipoNaturezaJuridica = (133, 1)

    campo match {
      case "codigoUnidadeGestora" => codigoUnidadeGestora
      case "codigoUnidadeOrcamentaria" => codigoUnidadeOrcamentaria
      case "denominacaoUnidadeOrcamentaria" => denominacaoUnidadeOrcamentaria
      case "nomeSecretarioMunicipal" => nomeSecretarioMunicipal
      case "cpfSecretarioMunicipal" => cpfSecretarioMunicipal
      case "atoAdministrativo" => atoAdministrativo
      case "tipoNaturezaJuridica" => tipoNaturezaJuridica
    }
  }

  def unidadeOrcamentariaLayout(linha: String, campo: String): String = substringLinha(linha, campoUOLayout(campo))

  def campoDotacaoLayout(campo: String): (Int, Int) = {
    val codigoUnidadeGestora = (0, 6)
    val anoVigenciaLeiOrcamentaria = (6, 4)
    val codigoUnidadeOrcamentaria = (10, 5)
    val codigoFuncao = (15, 2)
    val codigoSubFuncao = (17, 3)
    val codigoAcao = (24, 4)
    val codigoCategoriaEconomica = (34, 1)
    val codigoNaturezaDespesa = (35, 1)
    val codigoModalidadeAplicacao = (36, 2)
    val codigoElementoDespesa = (38, 2)
    val tipoFonteRecursos = (44,16)

    campo match {
      case "codigoUnidadeGestora" => codigoUnidadeGestora
      case "anoVigenciaLeiOrcamentaria" => anoVigenciaLeiOrcamentaria
      case "codigoUnidadeOrcamentaria" => codigoUnidadeOrcamentaria
      case "codigoFuncao" => codigoFuncao
      case "codigoSubFuncao" => codigoSubFuncao
      case "codigoAcao" => codigoAcao
      case "codigoCategoriaEconomica" => codigoCategoriaEconomica
      case "codigoNaturezaDespesa" => codigoNaturezaDespesa
      case "codigoModalidadeAplicacao" => codigoModalidadeAplicacao
      case "codigoElementoDespesa" => codigoElementoDespesa
      case "tipoFonteRecursos" => tipoFonteRecursos
    }
  }

  def dotacaoLayout(linha: String, campo: String): String = substringLinha(linha, campoDotacaoLayout(campo))

  def substringLinha(linha: String, tupla: (Int, Int)): String = {
    linha.substring(tupla._1, tupla._1 + tupla._2)
  }
}

object ControlesArquivo {

}