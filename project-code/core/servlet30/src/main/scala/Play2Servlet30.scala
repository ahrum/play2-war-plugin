package play.core.server.servlet30

import javax.servlet._
import javax.servlet.annotation._
import javax.servlet.http._
import java.io._
import java.net.URL
import java.util.concurrent.atomic._
import java.util.Arrays
import java.util.jar.JarFile

import org.jboss.vfs.VirtualFile
import org.reflections.ReflectionsException
import org.reflections.vfs._
import play.api._
import play.api.mvc._
import play.api.http._
import play.api.http.HeaderNames._
import play.api.libs.iteratee._
import play.api.libs.iteratee.Input._
import play.api.libs.concurrent._
import play.core._
import play.core.server.servlet._
import server.Server

import scala.collection.JavaConverters._

object Play2Servlet {
  val asyncTimeout = play.core.server.servlet.Play2Servlet.configuration.getInt("servlet30.asynctimeout").getOrElse(-1)
  Logger("play").debug("Async timeout for HTTP requests: " + asyncTimeout + " seconds")
}

@WebServlet(name = "Play", urlPatterns = Array { "/" }, asyncSupported = true)
@WebListener
class Play2Servlet extends play.core.server.servlet.Play2Servlet[Tuple2[AsyncContext, AsyncListener]] with Helpers {

  override def contextInitialized(e: ServletContextEvent) = {

    Vfs.addDefaultURLTypes(
      new Vfs.UrlType() {
        override def matches(url: URL) = {
          url.getProtocol.equals("vfs")
        }

        override def createDir(url: URL): Vfs.Dir = {
          var content: VirtualFile = {
            try {
              url.openConnection.getContent.asInstanceOf[VirtualFile]
            } catch {
              case e: Throwable => throw new ReflectionsException("could not open url connection as VirtualFile [" + url + "]", e)
            }
          }

          val dir = {
            try {
              val firstDir = createDir(new java.io.File(content.getPhysicalFile.getParentFile, content.getName))

              firstDir.orElse(createDir(content.getPhysicalFile))
            } catch {
              case e: Throwable => None
            }
          }

          dir.getOrElse(null)
        }

        def createDir(file: java.io.File): Option[Vfs.Dir] = {
          try {
            if (file.exists() && file.canRead()) {
              if (file.isDirectory()) {
                Option(new SystemDir(file))
              } else {
                Option(new ZipDir(new JarFile(file)))
              }
            } else {
              None
            }
          } catch {
            // TODO : log
            case e: IOException => None
          }
        }
      });

    // Then init main Servlet
    super.contextInitialized(e)
  }

  protected override def onBeginService(request: HttpServletRequest, response: HttpServletResponse): Tuple2[AsyncContext, AsyncListener] = {
    val asyncListener = new AsyncListener(request.toString)
    val asyncContext = request.startAsync
    asyncContext.setTimeout(play.core.server.servlet30.Play2Servlet.asyncTimeout);

    (asyncContext, asyncListener);
  }

  protected override def onFinishService(execContext: Tuple2[AsyncContext, AsyncListener]) = {
    // Nothing to do
  }

  protected override def onHttpResponseComplete(execContext: Tuple2[AsyncContext, AsyncListener]) = {
    execContext._1.complete
  }

  protected override def getHttpParameters(request: HttpServletRequest): Map[String, Seq[String]] = {
    Map.empty[String, Seq[String]] ++ request.getParameterMap.asScala.mapValues(Arrays.asList(_: _*).asScala)
  }

  protected override def getHttpRequest(execContext: Tuple2[AsyncContext, AsyncListener]): RichHttpServletRequest = {
    new RichHttpServletRequest {
      def getRichInputStream(): Option[InputStream] = {
        if (asyncContextAvailable(execContext._2)) {
          Option(execContext._1.getRequest.getInputStream)
        } else {
          None
        }
      }
    }
  }

  protected override def getHttpResponse(execContext: Tuple2[AsyncContext, AsyncListener]): RichHttpServletResponse = {
    new RichHttpServletResponse {
      def getRichOutputStream: Option[OutputStream] = {
        if (asyncContextAvailable(execContext._2)) {
          Option(execContext._1.getResponse.getOutputStream)
        } else {
          None
        }
      }

      def getHttpServletResponse: Option[HttpServletResponse] = {
        if (asyncContextAvailable(execContext._2)) {
          Option(execContext._1.getResponse.asInstanceOf[HttpServletResponse])
        } else {
          None
        }
      }
    }
  }

  private def asyncContextAvailable(asyncListener: AsyncListener) = {
    !asyncListener.withError.get && !asyncListener.withTimeout.get
  }
}

private[servlet30] class AsyncListener(val requestId: String) extends javax.servlet.AsyncListener {

  val withError = new AtomicBoolean(false)

  val withTimeout = new AtomicBoolean(false)

  // Need a default constructor for JBoss
  def this() = this("Unknown request id")

  override def onComplete(event: AsyncEvent) {
    // Logger("play").trace("onComplete: " + requestId)
    // Nothing
  }

  override def onError(event: AsyncEvent) {
    withError.set(true)
    Logger("play").error("Error asynchronously received for request: " + requestId, event.getThrowable)
  }

  override def onStartAsync(event: AsyncEvent) = {} // Nothing

  override def onTimeout(event: AsyncEvent) {
    withTimeout.set(true)
    Logger("play").warn("Timeout asynchronously received for request: " + requestId)
  }
}
