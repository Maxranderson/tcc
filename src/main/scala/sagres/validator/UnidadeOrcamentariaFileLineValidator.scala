package sagres.validator

import java.util.Calendar

import sagres.model.{ UnidadeOrcamentaria, ControleArquivo, ImportacaoException, UnidadesOrcamentarias, TipoErroImportacaoEnum}

import scala.util.{Failure, Success, Try}

class UnidadeOrcamentariaFileLineValidator(override protected val line: String,
                                           override protected val lineNumber: Int,
                                           override protected val ugArquivo: String,
                                           protected val calendarArquivo: Calendar)
                                          (controle: ControleArquivo, erros: ImportacaoException)
  extends FileLineValidator[UnidadeOrcamentaria] {

  private lazy val unidadeOrcamentariaBC = UnidadesOrcamentarias

  private def getCampo: String => String = controle.getCampo(line, _)

  private def getTamanho: String => Int = controle.getTamanho

  private lazy val codigoUnidGestoraStr = getCampo("codigoUnidadeGestora")
  private lazy val codigoUnidOrcamentariaStr = getCampo("codigoUnidadeOrcamentaria")
  private lazy val descricaoUnidOrcamentariaStr = getCampo("denominacaoUnidadeOrcamentaria")
  private lazy val nomeSecretarioStr = getCampo("nomeSecretarioMunicipal")
  private lazy val cpfSecretarioStr = getCampo("cpfSecretarioMunicipal")
  private lazy val atoJuridicoStr = getCampo("atoAdministrativo")
  private lazy val naturezaJuridicaStr = getCampo("tipoNaturezaJuridica")

  private lazy val anoNomeArquivo = calendarArquivo.get(Calendar.YEAR)

  private lazy val mesNomeArquivo = calendarArquivo.get(Calendar.MONTH) + 1

  /* --- Sobre as validações:
   * Nas lazy val a seguir, há apenas a validação da string recebida (ex: validação de tamanho, de conversão, etc)
   *
   * A validação se o campo existe mesmo ou não é feita no método hasEmptyFields()
   *
   * A validação com o banco é feita no validarLinha(unidadeOrcamentaria: UnidadeOrcamentaria)
   *
   */
  private lazy val codigoUnidGestora = {
      if(codigoUnidGestoraStr.substring(3,6) != ugArquivo.substring(3,6)) {
        erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Código da Unidade Gestora não pertence ao município ativo. [ UG: $codigoUnidGestoraStr ]", TipoErroImportacaoEnum.ERROR)
        None
      } else
        Option(codigoUnidGestoraStr)
  }

  private lazy val codigoUnidOrcamentaria = {
    if(!codigoUnidOrcamentariaStr.isEmpty){
      if(codigoUnidOrcamentariaStr.length == getTamanho("codigoUnidadeOrcamentaria"))
        Option(codigoUnidOrcamentariaStr)
      else {
        erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Código da Unidade Orçamentária menor que 5 caracteres. [ UO: $codigoUnidOrcamentariaStr ]", TipoErroImportacaoEnum.ERROR)
        None
      }
    } else
      None
  }

  private lazy val descricaoUnidOrcamentaria = {
    if (!descricaoUnidOrcamentariaStr.isEmpty)
      Option(descricaoUnidOrcamentariaStr)
    else
      None
  }

  private lazy val nomeSecretario = {
    if (!nomeSecretarioStr.isEmpty)
      Option(nomeSecretarioStr)
    else
      None
  }

  private lazy val cpfSecretario = try {
    if (!cpfSecretarioStr.isEmpty) {
      cpfSecretarioStr.toLong // testa se são todos dígitos numéricos
      if (cpfSecretarioStr.length == getTamanho("cpfSecretarioMunicipal"))
        Option(cpfSecretarioStr)
      else {
        erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"CPF do Secretário inválido. [ CPF: $cpfSecretarioStr ]", TipoErroImportacaoEnum.ERROR)
        None
      }
    }
    else
      None
  } catch {
    case _: NumberFormatException => {
      erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"CPF inválido. Apenas números. [ CPF: $cpfSecretarioStr ]", TipoErroImportacaoEnum.ERROR)
      None
    }
  }

  private lazy val atoJuridico = try {
    if (!atoJuridicoStr.isEmpty) {
        atoJuridicoStr.toLong // confere se é apenas números
        Some(atoJuridicoStr)
    }
    else
      None
  }
  catch {
    case _: NumberFormatException => {
      erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Tipo de Ato Jurídico inválido. [ Tipo: $atoJuridicoStr ]", TipoErroImportacaoEnum.ERROR)
      None
    }
  }

  private lazy val naturezaJuridica = try {
    if (!naturezaJuridicaStr.isEmpty) {
      naturezaJuridicaStr.toLong // confere se são todos números
      Option(naturezaJuridicaStr)
    }
    else
      None
  } catch {
    case _: NumberFormatException => {
      erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Tipo de Natureza Jurídica inválido. [ Tipo: $naturezaJuridicaStr ]", TipoErroImportacaoEnum.ERROR)
      None
    }
  }

  private lazy val unidadeOrcamentaria: Option[UnidadeOrcamentaria] = {
    Option(
      UnidadeOrcamentaria(
        None,
        codigoUnidGestora.getOrElse("0"),
        anoNomeArquivo,
        mesNomeArquivo,
        codigoUnidOrcamentaria.getOrElse("0"),
        descricaoUnidOrcamentaria,
        nomeSecretario,
        cpfSecretario,
        atoJuridico,
        naturezaJuridica
      )
    )
  }

  /**
    * Recebe uma função para verificar se a entidade não está duplicada no arquivo. Valida a linha do arquivo.
    *
    * @return
    * Option[T] caso a validação não encontre erros.
    */
  override def getEntidade(testarDuplicidadeNoArquivo: Option[UnidadeOrcamentaria] => Boolean): Option[UnidadeOrcamentaria] = {
    if (line.length == controle.tamanhoLinha) {
      if(!Jurisdicionado.isPrefeitura(ugArquivo) && codigoUnidGestora.getOrElse("0") != ugArquivo) {
        erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Unidade Gestora inválida. [ UG: ${codigoUnidGestora.getOrElse("não informado")} ]", TipoErroImportacaoEnum.ERROR)
        None
      } else {
        if(hasEmptyFields) {
          None
        } else {
          if(testarDuplicidadeNoArquivo(unidadeOrcamentaria)) {
            erros.adicionarErro(controle.codigoArquivo, lineNumber, line, "Unidade Orçamentária duplicada no arquivo. ", TipoErroImportacaoEnum.ERROR)
            None
          } else {
            if(validarLinha(unidadeOrcamentaria.get))
              unidadeOrcamentaria
            else
              None
          }
        }
      }
    } else {
      erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Tamanho de linha inválido. [ Tamanho atual: ${line.length.toString} - Tamanho esperado: ${controle.tamanhoLinha.toString} ]", TipoErroImportacaoEnum.ERROR)
      None
    }
  }

  /**
    * Valida os campos da entidade que precisam existir no banco.
    * @param unidOrcamentaria entidade a ter os campos validados
    */
  private def validarLinha(unidOrcamentaria: UnidadeOrcamentaria): Boolean = {
    val existsUnidadeOrcamentaria = unidadeOrcamentariaBC.findUnique(unidOrcamentaria.unidadeGestora, unidOrcamentaria.codigoUnidOrcamentaria, unidOrcamentaria.ano).isDefined
    val existsNaturezaJuridica = TiposNaturezaJuridica.existsById(unidOrcamentaria.naturezaJuridica.get.toLong)
    val existsAtoJuridico = TiposAtoJuridico.existsById(unidOrcamentaria.atoJuridico.get.toLong)
    val ugFromCB = CaboBrancoService.getJurisdicionadoByNumeroUG(unidOrcamentaria.unidadeGestora) match {
      case Success(result) => {
        if (!result.isEmpty) {
          if (codigoUnidGestoraStr.length == getTamanho("codigoUnidadeGestora")) {
            if (ugArquivo.substring(0, 3).contentEquals("201")) {
              if (codigoUnidGestoraStr.substring(0, 3).contentEquals("101") || codigoUnidGestoraStr.substring(0, 3).contentEquals("701")) {
                erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Código da Unidade Gestora não permitido. [ Código: $codigoUnidGestoraStr ]", TipoErroImportacaoEnum.ERROR)
                "0"
              } else {
                codigoUnidGestoraStr
              }
            } else {
              codigoUnidGestoraStr
            }
          } else {
            erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Unidade Gestora não habilitada no sistema. [ Código: $codigoUnidGestoraStr ]", TipoErroImportacaoEnum.ERROR)
            "0"
          }
        } else {
          erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Unidade Gestora não habilitada no sistema. [ Código: $codigoUnidGestoraStr ]", TipoErroImportacaoEnum.ERROR)
          "0"
        }
      }
      case Failure(e) => throw e
    }

    if(existsUnidadeOrcamentaria) {
      erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Unidade Orçamentária já cadastrada no sistema. " +
                            s"[ UG: $codigoUnidGestoraStr - UO: $codigoUnidOrcamentariaStr - Ano: ${anoNomeArquivo.toString} ]", TipoErroImportacaoEnum.ERROR)
    }

    if(!existsNaturezaJuridica) {
      erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Tipo de Natureza Jurídica inválido. [ Tipo: $naturezaJuridicaStr ]", TipoErroImportacaoEnum.ERROR)
    }

    if(!existsAtoJuridico) {
      erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Tipo de Ato Jurídico inválido. [ Tipo: $atoJuridicoStr ]", TipoErroImportacaoEnum.ERROR)
    }

    if(!existsAtoJuridico || !existsNaturezaJuridica || existsUnidadeOrcamentaria || ugFromCB.contentEquals("0"))
      false
    else
      true
  }

  /**
    * Verifica se campos obrigatórios estão devidamente preenchidos
    *
    * @return `true` se houver algum campo vazio, ou `false` se todos estiverem preenchidos
    */
  private def hasEmptyFields: Boolean = {
    if(codigoUnidGestoraStr.isEmpty || codigoUnidOrcamentariaStr.isEmpty || descricaoUnidOrcamentariaStr.isEmpty ||
      nomeSecretarioStr.isEmpty || cpfSecretarioStr.isEmpty || atoJuridicoStr.isEmpty || naturezaJuridicaStr.isEmpty || line.substring(134, 140).trim.isEmpty) {

      if(codigoUnidGestoraStr.isEmpty) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, "Campo Unidade Gestora não pode ser vazio.", TipoErroImportacaoEnum.ERROR)
      if(codigoUnidOrcamentariaStr.isEmpty) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, "Campo Código da Unidade não pode ser vazio.", TipoErroImportacaoEnum.ERROR)
      if(descricaoUnidOrcamentariaStr.isEmpty) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, "Campo Descrição da Unidade Orçamentária não pode ser vazia.", TipoErroImportacaoEnum.ERROR)
      if(nomeSecretarioStr.isEmpty) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, "Campo Nome do Secretário não pode ser vazio.", TipoErroImportacaoEnum.ERROR)
      if(cpfSecretarioStr.isEmpty) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, "Campo CPF do Secretário não pode ser vazio.", TipoErroImportacaoEnum.ERROR)
      if(atoJuridicoStr.isEmpty) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, "Campo Tipo de Ato Jurídico não pode ser vazio.", TipoErroImportacaoEnum.ERROR)
      if(naturezaJuridicaStr.isEmpty) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, "Campo Tipo de Natureza Jurídica não pode ser vazio.", TipoErroImportacaoEnum.ERROR)
      if(line.substring(134,140).trim.isEmpty) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, "Campo RESERVADO não pode ser vazio. Preencha com 0.", TipoErroImportacaoEnum.ERROR)

      true

    } else false
  }
}
