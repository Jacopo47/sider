package com.sider.api.options

sealed trait ExpireOption {
  val command: String
}
case class NX(override val command: String = "NX") extends ExpireOption {}
case class XX(override val command: String = "XX") extends ExpireOption {}
case class GT(override val command: String = "GT") extends ExpireOption {}
case class LT(override val command: String = "LT") extends ExpireOption {}