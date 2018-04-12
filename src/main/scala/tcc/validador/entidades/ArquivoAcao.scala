package tcc.validador.entidades

case class ArquivoAcao(
                        codigoUnidadeGestora: String,
                        codigoAcao: String,
                        denominacaoAcao: String,
                        tipoAcao: String,
                        descricaoMeta: String,
                        unidadeMedida: String
                      ) extends EntidadeArquivo with UnidadeGestoraRel

object ArquivoAcao {

}
