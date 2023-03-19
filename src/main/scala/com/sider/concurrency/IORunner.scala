package com.sider.concurrency

object IORunner {
  def runUnsafe[A](input: IO[A]): A = input.get
}
