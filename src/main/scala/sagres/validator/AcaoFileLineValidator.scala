package sagres.validator

import java.sql.Date
import java.util.Calendar

import sagres.model._
import sagres.service.CaboBrancoService

import scala.util.{Failure, Success}
import sagres.utils.StringUtils._

class AcaoFileLineValidator(override protected val line: String,
                               override protected val ugArquivo: String,
                               override protected val lineNumber: Int,
                               protected val dataCompetenciaArquivo: Date)(controle: ControleArquivo, erros: ImportacaoException) extends FileLineValidator[Acao] {
	
	
	private def getCampo: String => String = controle.getCampo(line, _)
	//Variáveis brutas
	private lazy val codigoUnidadeGestoraStr = getCampo("codigoUnidadeGestora")
	private lazy val codigoAcaoStr = getCampo("codigoAcao")
	private lazy val denominacaoAcaoStr = Some(getCampo("denominacaoAcao"))
	private lazy val tipoAcaoStr = Some(getCampo("tipoAcao"))
	private lazy val descricaoMetaStr = Some(getCampo("descricaoMeta"))
	private lazy val unidadeMedidaStr = Some(getCampo("unidadeMedida"))
	
	//Variável de mensagem para identificar a ação
	private lazy val msgIdentificadora = s"[ Código ação: ${codigoAcaoStr.ifIsEmpty("Não informado")} ]"
	
	//Variáveis tratadas, métodos com get são os que devem adicionar erros
	private lazy val calendar: Calendar = {
		val calendar = Calendar.getInstance()
		
		calendar.setTime(dataCompetenciaArquivo)
		
		calendar
	}
	
	private lazy val ano: Int = {
		calendar.get(Calendar.YEAR)
	}
	
	private lazy val mesEnvio: Int = {
		calendar.get(Calendar.MONTH) + 1
	}
	
	private lazy val getUnidadeGestora = {
		val ugCod = codigoUnidadeGestoraStr.substring(0, 3)
		val municipioCod = codigoUnidadeGestoraStr.substring(3, 6)
		val ugArq = ugArquivo.substring(0, 3)
		val municipioArquivo = ugArquivo.substring(3, 6)
		val ugsExcluidas = Array("701", "101")
		val ehPrefeitura = ugArq == "201"

		
		val ugEncontrada: String = CaboBrancoService.getJurisdicionadoByNumeroUG(codigoUnidadeGestoraStr) match {
				case Success(result) => {
					result
				}
				
				case Failure(e) => throw e
			}

		if(!ugEncontrada.contains(codigoUnidadeGestoraStr)) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Unidade Gestora não existe em nosso sistema: ${codigoUnidadeGestoraStr.ifIsEmpty("Não informado")}. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
		
		if(municipioCod != municipioArquivo) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Unidade Gestora não pertence ao município atual: ${codigoUnidadeGestoraStr.ifIsEmpty("Não informado")}. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
		
		if(ehPrefeitura && ugsExcluidas.contains(ugCod)) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Prefeitura não pode enviar ações da Câmara ou de Consórcios: ${codigoUnidadeGestoraStr.ifIsEmpty("Não informado")}. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
		
		if(!ehPrefeitura && ugArquivo != codigoUnidadeGestoraStr) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Unidade Gestora inválida: ${codigoUnidadeGestoraStr.ifIsEmpty("Não informado")}. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
		
		if (codigoUnidadeGestoraStr.isEmpty) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Código da Unidade Gestora deve ser informado. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
		
		codigoUnidadeGestoraStr
	}
	
	private lazy val getCodigoAcao = {
		
		if(codigoAcaoStr.isEmpty) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Código da ação deve ser informado. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
		
		else{
		
		if(codigoAcaoStr.length < 4) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Código da ação não deve ser inferior a 4 caracteres. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
		
		}
		
		
		codigoAcaoStr
	}
	
	private lazy val getDenominacaoAcao = {
		val minDescricao = 10
		if(denominacaoAcaoStr.get.length < minDescricao) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Descrição da ação não deve ser inferior a $minDescricao caracteres. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
		denominacaoAcaoStr
	}
	
	private lazy val getTipoAcao = {
		
		tipoAcaoStr
	}
	
	private lazy val getDescricaoMeta = {
		
		descricaoMetaStr
	}
	
	private lazy val getUnidadeMedida = {
		
		unidadeMedidaStr
	}
	
	private lazy val getAcao: Option[Acao] = {
		Some(Acao(None, getUnidadeGestora, ano, getCodigoAcao, getDenominacaoAcao, getTipoAcao, getUnidadeMedida, getDescricaoMeta, mesEnvio))
	}
	
	private def afterValidateLine(calendarDataEnvioArquivo: Calendar)(controle: ControleArquivo, erros: ImportacaoException) = {
		val acaoBC = Acoes
		val tipoAcaoBC = TiposAcao
		
		val acao = getAcao
		
		val acaoExistente = acaoBC.findUnique(getUnidadeGestora, getCodigoAcao, ano)
		if(acaoExistente.isDefined){
			erros.adicionarErro(controle.codigoArquivo, lineNumber, line, "Ação já cadastrada no sistema", TipoErroImportacaoEnum.ERROR)
		}else{
			
			val tipoAcaoExistente = tipoAcaoBC.findUnique(acao.get.tipoAcao.get)
			if(tipoAcaoExistente.isEmpty) erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Código do tipo da ação inválida. $msgIdentificadora", TipoErroImportacaoEnum.ERROR)
			
		}
		
	}
	
	override def getEntidade(testIfExistsInFile: Option[Acao] => Boolean): Option[Acao] = {
		
		if(line.length == controle.tamanhoLinha){
			
			if(getUnidadeGestora.substring(3, 6) != ugArquivo.substring(3, 6) || getCodigoAcao.isEmpty){
				
				None
			}else{
				
				if (testIfExistsInFile(Some(getAcao.get))) {
					erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Ação duplicada no arquivo", TipoErroImportacaoEnum.ERROR)
					None
					
				} else {
					afterValidateLine(calendar)
					getAcao
				}
				
			}
			
		}else{
			
			erros.adicionarErro(controle.codigoArquivo, lineNumber, line, s"Tamanho da linha inválido. [ Tamanho válido: ${controle.tamanhoLinha} - Tamanho atual: ${line.length}]", TipoErroImportacaoEnum.ERROR)
			None
			
		}
		
	}
	
}
