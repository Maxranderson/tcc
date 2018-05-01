package tcc

import java.io.File

object Metricas {

  def tempoExecucaoComResultado[R](bloco: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = bloco
    val t1 = System.currentTimeMillis()
    println(s"O tempo foi: ${t1-t0} ms")
    result
  }

  def tempoExecucaoComResultados[R](bloco: => R): (R, Long) = {
    val t0 = System.currentTimeMillis()
    val result = bloco
    val t1 = System.currentTimeMillis()
    (result, t1-t0)
  }

  def tempoExecucaoPorArquivoEhQuantidadeLinha[R](bloco: (File) => R): String = {
    val quantidade = 1 to 15
    val arquivos = quantidade.map(vez => new File(s"src/main/resources/arquivos-testes/vezes$vez.txt"))
    val resultados = arquivos.map {
      arquivo =>
        Metricas.tempoExecucaoComResultados {
          bloco(arquivo)
        }
    }

    val resultadosPorLinha = resultados.zipWithIndex.foldLeft("") {
      (acumulador, proximo) =>
        val ((res, tempo), indice) = proximo
        acumulador + "\n" + s"Resultado de ${(indice + 1) * 400} linhas: $tempo ms"
    }
   resultadosPorLinha + "\n" + s"A média foi: ${resultados.foldLeft(0.toDouble)(_ + _._2) / resultados.length} ms"
  }

}
