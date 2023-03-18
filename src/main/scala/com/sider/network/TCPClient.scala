package com.sider.network

import io.netty.channel.ChannelInboundHandlerAdapter
import io.netty.channel.ChannelHandlerContext
import io.netty.buffer.ByteBuf
import scala.util.Try
import com.sider.bytesToString
import io.netty.channel.EventLoopGroup
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.bootstrap.Bootstrap
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.channel.ChannelOption
import io.netty.channel.ChannelInitializer
import io.netty.channel.socket.SocketChannel
import io.netty.channel.ChannelFuture
import org.slf4j.Logger
import org.slf4j.LoggerFactory


class TCPClient(
    val host: Option[String] = Some("localhost"),
    val port: Option[Int] = Some(6379)
) {

    val logger: Logger = LoggerFactory.getLogger(classOf[TCPClient])

    val worker: EventLoopGroup = new NioEventLoopGroup()

    Try {
        val bootstrap = new Bootstrap()
        bootstrap.group(worker)
        bootstrap.channel(classOf[NioSocketChannel])
        bootstrap.option(ChannelOption.SO_KEEPALIVE, true)
        bootstrap.handler(new ChannelInitializer[SocketChannel] {
            override protected def initChannel(ch: SocketChannel): Unit =
                logger.debug("Initializing channel")
                ch.pipeline().addLast(new Resp3Handler())
        })


        val app: Either[Throwable, ChannelFuture] = (host, port) match {
            case (Some(h), Some(p)) => Right(bootstrap.connect(h, p))
                case _ => Left(Throwable("Host and/or port not valid"))
        } map(_.sync())

        app.map(e =>
            logger.debug("Listening")
            e
        ).map(_.channel()).map(_.closeFuture()).map(_.sync())

    }

    worker.shutdownGracefully()

}


class Resp3Handler extends ChannelInboundHandlerAdapter {

    val logger = LoggerFactory.getLogger(classOf[Resp3Handler])

    override def channelActive(ctx: ChannelHandlerContext): Unit = ctx.writeAndFlush("*1\r\n$4\r\nPING\r\n")

    override def channelRead(ctx: ChannelHandlerContext, msg: Object): Unit = {
        val m: ByteBuf = msg.asInstanceOf[ByteBuf]

        ctx.writeAndFlush("*1\r\n$4\r\nPING\r\n")

        Try {
            val bytes = m.readBytes(m.readableBytes()).array()
            logger.info("Printing: {} :: as string :: {}", bytes, bytes.toSeq.bytesToString)

            ctx.close()
        }

        m.release()
    }

    override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
        cause.printStackTrace()
        ctx.close()
    }

}