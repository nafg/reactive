import org.mortbay.jetty.Connector
import org.mortbay.jetty.Server
import org.mortbay.jetty.webapp.WebAppContext
import org.mortbay.component.LifeCycle
import org.mortbay.jetty.nio._

object RunWebApp extends App {
  val server = new Server
  val scc = new SelectChannelConnector
  scc.setPort(8080)
  server.setConnectors(Array(scc))

  val context = new WebAppContext("src/main/webapp", "/")
  context.setServer(server)
//  context.setContextPath("/")
//  context.setWar("src/main/webapp")

  server.addHandler(context)
  Class.forName("net.liftweb.http.LiftRules$")
  try {
    println(">>> STARTING EMBEDDED JETTY SERVER, PRESS ANY KEY TO STOP")
    server.start
    context.start
    println(context.isStarted)
    println(context.isFailed)
    Console.readLine
//    while (System.in.available() == 0) {
//      Thread.sleep(5000)
//    }
    server.stop()
//    server.join
  } catch {
    case exc : Exception => {
      exc.printStackTrace()
      System.exit(100)
    }
  }
}
