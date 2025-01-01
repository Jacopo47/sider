package com.sider.api.options

sealed trait ListInsertPositionOption { 
  val command: String
}

case class BEFORE(override val command: String = "BEFORE") extends ListInsertPositionOption {}
case class AFTER(override val command: String = "AFTER") extends ListInsertPositionOption {}
