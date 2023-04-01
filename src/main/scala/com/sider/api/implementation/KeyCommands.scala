package com.sider.api.implementation

import com.sider.api.KeyCommands
import com.sider.network.Resp3TcpClient
import com.sider.BlobString

class BasicKeyCommands(
    val tcp: Resp3TcpClient
) extends KeyCommands {


  given global: Resp3TcpClient = tcp

  override def copy(source: String, dest: String, db: Option[String] = None, replace: Boolean = false): Either[Throwable, Long] =
    var commands = Array("COPY", source, dest)

    commands = if (db.isDefined) then commands :+ "DB" :+ db.get else commands
    commands = if (replace) then commands :+ "REPLACE" else commands  

    sendCommandWithGenericErrorHandler(commands) {
      case v: com.sider.Number => v.value
    }

  override def del(key: String*): Either[Throwable, Long] = 
    sendCommandWithGenericErrorHandler("DEL" +: key.toArray) {
      case v: com.sider.Number => v.value
    }

  override def dump(key: String): Either[Throwable, String] =
    sendCommandWithGenericErrorHandler(Array("DUMP", key)) {
      case v: BlobString => v.value
    }

  override def restore(key: String, serializedValue: String, ttl: Option[Long] = Some(0), replace: Boolean = false, absTtl: Boolean = false, idleTime: Option[Long] = None, freq: Option[Long] = None): Either[Throwable, String] =
    var commands = Array("RESTORE", key, ttl.getOrElse(0L), serializedValue)

    commands = if (replace) then commands :+ "REPLACE" else commands  
    commands = if (absTtl) then commands :+ "ABSTTL" else commands
    commands = if (idleTime.isDefined) then commands :+ "IDLETIME" :+ idleTime.get else commands
    commands = if (freq.isDefined) then commands :+ "FREQ" :+ idleTime.get else commands

    sendCommandWithGenericErrorHandler(commands) {
      case v: com.sider.SimpleString => v.value
    }  


  override def exists(key: String*): Either[Throwable, Long] = 
    sendCommandWithGenericErrorHandler("EXISTS" +: key.toArray) {
      case v: com.sider.Number => v.value
    }
}
