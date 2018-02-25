
package sagres.validator

import java.sql.Date
import java.util.Calendar

import sagres.model._
import sagres.utils.StringUtils._

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}

class ProgramaFileLineValidator(override protected val line: String,
                            override protected val ugArquivo: String,
                            override protected val lineNumber: Int,
                            protected val dataCompetenciaArquivo: Date)(controle: ControleArquivo, erros: ImportacaoException) extends FileLineValidator[Programa] {
	private lazy val programaBC = Programas
	private lazy val tipoObjetivoMilenioBC = new TipoObjetivoMilenioBC
	
	private def getCampo: String => String = controle.getCampo(line, _)
	//Variáveis brutas
	private lazy val codigoUnidadeGestoraStr = getCampo("codigoUnidadeGestora")
	private lazy val codigoProgramaStr = getCampo("codigoPrograma")
	private lazy val denominacaoProgramaStr = getCampo("denominacaoPrograma")
	private lazy val descricaoObjetivoProgramaStr = getCampo("descricaoObjetivoPrograma")
	private lazy val tipoObjetivoMilenio = getCampo("tipoObjetivoMilenio")
	
	//Variável de mensagem para identificar a programa
	
	private lazy val msgIdentificadora = s"[ Código programa: ${codigoProgramaStr.ifIsEmpty("Não informado")} ]"
	
	//Variáveis tratadas, métodos com get são os que devem adicionar erros
	
	private lazy val calendar = {
		val calendar = Calendar.getInstance()
		calendar.setTime(dataCompetenciaArquivo)
		calendar
	}
	
	private lazy val getCodigoUnidadeGestora = {
		val ugCod = codigoUnidadeGestoraStr.substring(0, 3)
		val municipioCod = codigoUnidadeGestoraStr.substring(3, 6)
		val ugArq = ugArquivo.substring(0, 3)
		val municipioArquivo = ugArquivo.substring(3, 6)
		val ugsExcluidas = Array("701", "101")
		val ehPrefeitura = ugArq == "201"
		
		val ugsExistente = ListBuffer[String]()
		val ugsInexistente = ListBuffer[String]()
		
		if(!ugsExistente.contains(codigoUnidadeGestoraStr) && !ugsInexistente.contains(codigoUnidadeGestoraStr)){
			CaboBrancoService.getJurisdicionadoByNumeroUG(codigoUnidadeGestoraStr) match {
				case Success(result) => {
					if(!result.isEmpty) ugsExistente += codigoUnidadeGestoraStr
					else ugsInexistente += codigoUnidadeGestoraStr
				}
				
				case Failure(e) => throw e
			}
		}
		if(ugsInexistente.contains(codigoUnidadeGestoraStr)) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Unidade Gestora não existe em nosso sistema: ${codigoUnidadeGestoraStr.ifIsEmpty("Não informado")}. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
		
		
		if(municipioCod != municipioArquivo) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Unidade Gestora não pertence ao município atual: ${codigoUnidadeGestoraStr.ifIsEmpty("Não informado")}. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
		
		if(ehPrefeitura && ugsExcluidas.contains(ugCod)) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Prefeitura não pode enviar programas da Câmara ou de Consórcios: ${codigoUnidadeGestoraStr.ifIsEmpty("Não informado")}. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
		
		if(!ehPrefeitura && ugArquivo != codigoUnidadeGestoraStr) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Unidade Gestora inválida: ${codigoUnidadeGestoraStr.ifIsEmpty("Não informado")}. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
		
		if (codigoUnidadeGestoraStr.isEmpty) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Código da Unidade Gestora deve ser informado. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
		
		codigoUnidadeGestoraStr
	}
	
	private lazy val getCodigoPrograma = {
		
		if(codigoProgramaStr.length < 4) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Código do programa deve ser informado e não pode ser menor que 4 caracteres. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
		
		codigoProgramaStr
	}
	
	private lazy val getDenominacaoPrograma = {
		
		if(denominacaoProgramaStr.isEmpty) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Denominação do programa deve ser informado. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
		
		denominacaoProgramaStr
	}
	
	private lazy val getDescricaoObjetivoPrograma = {
		
		if(descricaoObjetivoProgramaStr.isEmpty) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Descrição do objetivo do programa deve ser informado. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
		
		descricaoObjetivoProgramaStr
	}
	
	private lazy val getTipoObjetivoMilenio: Int = {
		try{
			tipoObjetivoMilenio.toInt
		}catch {
			case e : Exception => {
				erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Tipo objetivo milênio deve conter somente números. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
				-1
			}
		}
	}
	
	private lazy val getAno = {
		calendar.get(Calendar.YEAR)
	}
	
	private lazy val getMesEnvio = {
		calendar.get(Calendar.MONTH) + 1
	}
	
	private lazy val getPrograma: Option[Programa] = {
		
		val programa = Some(Programa(None, getCodigoUnidadeGestora, getAno, getCodigoPrograma, Some(getDenominacaoPrograma), Some(getDescricaoObjetivoPrograma), Some(getTipoObjetivoMilenio), Some(getMesEnvio)))
		programa
		
	}
	
	private def afterValidateLine(calendarDataEnvioArquivo: Calendar)(implicit s: Session, controle: ControleArquivo, erros: ImportacaoException) = {
	
		val programa = getPrograma
		
		val programaExistente = programaBC.findUnique(getCodigoUnidadeGestora, getAno, getCodigoPrograma)
		if(programaExistente.isDefined){
			erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Programa já cadastrado no sistema. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
		}else {
		
			val tipoObjetivoMilenioExistente = tipoObjetivoMilenioBC.findById(getTipoObjetivoMilenio)
			if(tipoObjetivoMilenioExistente.isEmpty) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Tipo Objetivo do Milênio inválido. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
		
		}
		
	}
	
	override def getEntidade(testIfExistsInFile: Option[Programa] => Boolean): Option[Programa] = {
		
		if(line.length == controle.tamanhoLinha){
			
			if(getCodigoUnidadeGestora.substring(3, 6) != ugArquivo.substring(3, 6) || getCodigoPrograma.isEmpty){
				
				None
			}else{
				
				if (testIfExistsInFile(Some(getPrograma.get))) {
					erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Programa duplicado no arquivo. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
					None
					
				} else {
					afterValidateLine(calendar)
					getPrograma
				}
				
			}
			
		}else{
			
			erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Tamanho da linha inválido. [ Tamanho válido: ${controle.tamanhoLinha} - Tamanho atual: ${line.length}]", TipoErroImportacaoEnum.ERROR)
			None
			
		}
		
	}
	
}
