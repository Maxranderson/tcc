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
    val numerosArquivos = 1 to 15
    val arquivos = numerosArquivos.map(numero => (numero, new File(s"src/main/resources/arquivos-testes/vezes$numero.txt")))
    val resultados = arquivos.map {
      tuplaNumeroArquivo =>
        val (numero, arquivo) = tuplaNumeroArquivo
        (Metricas.tempoExecucaoComResultados {
          bloco(arquivo)
        }, numero)
    }
    val resultadosPorLinha = resultados.foldLeft("") {
      (acumulador, proximo) =>
        val ((res, tempo), numero) = proximo
        acumulador + "\n" + s"Resultado de ${numero * 400} linhas: $tempo ms"
    }
    resultadosPorLinha + "\n" + s"O tempo total foi: ${resultados.foldLeft(0.toLong)(_ + _._1._2)} ms"
  }

}
