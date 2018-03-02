package tcc

object Metricas {
  def tempoExecucaoComResultado[R](bloco: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = bloco
    val t1 = System.currentTimeMillis()
    println(s"O tempo foi: ${t1-t0} ms")
    println(s"O resultado: $result")
    result
  }
}
