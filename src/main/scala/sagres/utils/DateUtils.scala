package sagres.utils

import java.sql.Date
import java.text.SimpleDateFormat
import java.util.Calendar

/**
  * Created by rguimaraes on 28/03/2017.
  */
object DateUtils {
  def stringToSqlDate(strdate: String) = {
    try {
      if (strdate.nonEmpty) {
        if(strdate.length == 8) {
          val sdf = new SimpleDateFormat("ddMMyyyy")
          sdf.setLenient(false)
          Some(new Date(sdf.parse(strdate).getTime))
        } else if(strdate.length == 6){
          val sdf = new SimpleDateFormat("MMyyyy")
          sdf.setLenient(false)
          Some(new Date(sdf.parse(strdate).getTime))
        } else None
      } else {
        None
      }
    }catch{
      case e: Exception => None
    }
  }

  def todayIsBetween(start: Date, end: Date) : Boolean = {
    val calendarInicial = Calendar.getInstance()
    calendarInicial.setTime(start)
    calendarInicial.add(Calendar.DATE, -1)

    val calendaFinal = Calendar.getInstance()
    calendaFinal.setTime(end)
    calendaFinal.add(Calendar.DATE, 1)

    val hoje = Calendar.getInstance()

    hoje.after(calendarInicial) && hoje.before(calendaFinal)
  }

  def subtract(date: Date, dias: Int) : Date = {
    val cal = Calendar.getInstance()
    cal.setTime(date)
    cal.add(Calendar.DATE, -1)
    new Date(cal.getTime.getTime)
  }

  def getMes(date: Date) : Int = {
    val cal = Calendar.getInstance()
    cal.setTime(date)
    cal.get(Calendar.MONTH) +1
  }

  def currentTimeStr : String = {
    val formatter = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss")
    formatter.format(Calendar.getInstance().getTime)
  }

}
