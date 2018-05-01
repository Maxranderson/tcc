import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.io.{Source, StdIn}
import scala.util.Try

object GerarArquivos extends App {
  def concatListByNum(vezes: Int, lista: Seq[String]): Seq[String] = {
    (1 to vezes).map(v => lista).reduce(_ ++: _)
  }

  def listaToFile(nome: String, lista: Seq[String]): File = {
    Files.write(Paths.get(s"src/main/resources/arquivos-testes/$nome"),lista.reduce(_ + "\n" + _).getBytes(StandardCharsets.UTF_8))
    new File(nome)
  }
  println("Digite a quantidade de vezes que as linhas serão multiplicadas:")
  val input = StdIn.readInt()
  val listaArquivo = Source.fromResource("201095012018Acao.txt").getLines.toList
  input match {
    case p if p > 0 => Try(
      Range.inclusive(1, p).foreach(quant => listaToFile(s"vezes$quant.txt", concatListByNum(quant, listaArquivo)))
      ).map(v => s"Arquivo com $p gerado com sucesso!").fold(
      e => println(e.getMessage),
      s => println(s)
    )
    case _ =>
      listaToFile(s"vezes1.txt", concatListByNum(1, listaArquivo))
      println("Arquivo com 1 gerado com sucesso!")
  }
}
