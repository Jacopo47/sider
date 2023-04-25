package com.sider.api.entities

final case class ScanResponse(
  next: String,
  elements: Seq[String]
)
