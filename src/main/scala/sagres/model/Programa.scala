package sagres.model

/**
  * Created by dformiga on 11/10/2016.
  */

import java.sql.Date
import java.util.Calendar

case class Programa (id:Option[Long],
                     unidadeGestora: String,
                     ano: Int,
                     codigoPrograma: String,
                     denominacaoPrograma: Option[String],
                     descricaoObjetivo: Option[String],
                     objetivoMilenio: Option[Int],
                     mesEnvio: Option[Int],
                     confirmado: Boolean = false,
                     createdAt: Date = new Date(Calendar.getInstance.getTime.getTime),
                     var cancelled: Option[Long] = None
                    ){
  def equalsTo(obj: Programa) = {
    obj.codigoPrograma == codigoPrograma &&
        obj.ano == ano &&
        obj.unidadeGestora == unidadeGestora
  }
}

object Programas {
  def findUnique(unidadeGestora: String, ano: Int, codigoPrograma: String): Option[Programa] = {
    None
  }
}

case class TipoObjetivoMilenio(id: Int, descricao: String)

object TipoObjetivoMilenio {
  val tipos = Seq(
    TipoObjetivoMilenio(1,"Acabar com a Fome e a Miséria"),
    TipoObjetivoMilenio(2,"Educação Básica de Qualidade para todos"),
    TipoObjetivoMilenio(3,"Igualdade entre Sexos e Valorização da Mulher"),
    TipoObjetivoMilenio(4,"Reduzir a Mortalidade Infantil"),
    TipoObjetivoMilenio(5,"Melhorar a Saúde das Gestantes"),
    TipoObjetivoMilenio(6,"Combater a Aids, a Malária e outras doenças"),
    TipoObjetivoMilenio(7,"Qualidade de Vida e Respeito ao Meio Ambiente"),
    TipoObjetivoMilenio(8,"Todo Mundo trabalhando pelo Desenvolvimento"),
    TipoObjetivoMilenio(9,"Outros Objetivos")
  )
  def findById(id: Int): Option[TipoObjetivoMilenio] = {
    tipos.find(_.id == id)
  }
}