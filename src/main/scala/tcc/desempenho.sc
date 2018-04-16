import tcc.Metricas

import scala.util.{Failure, Success, Try}

val teste = (1 to 400).toList

def converteParaTry( valor: Int): Try[Int] = {
  Thread.sleep(1)
  if(valor == 200) {
    Failure(new Exception("Falhou"))
  } else {
    Try(valor)
  }
}

def processarLista(seq: List[Int]): List[Try[Int]] = {
  def loop(listaAhProcessar: List[Int], novaLista: List[Try[Int]]): List[Try[Int]] = {
    listaAhProcessar match {
      case Nil => novaLista
      case atual :: proximos =>
        val resultado = converteParaTry(atual)
        resultado match {
          case Failure(e) => Failure(e) +: novaLista
          case Success(s) => loop(proximos, Success(s) +: novaLista)
        }
    }
  }
  loop(seq, List())
}

Metricas.tempoExecucaoComResultado {
  teste.map(converteParaTry)
}

Metricas.tempoExecucaoComResultado {
  teste.par.map(converteParaTry).toList
}

Metricas.tempoExecucaoComResultado {
  processarLista(teste)
}

