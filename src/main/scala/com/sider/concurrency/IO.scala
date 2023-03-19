package com.sider.concurrency

class IO[+A](value: => A) {
    private lazy val internal: A = value

    def flatMap[B](f: (=> A) => IO[B]): IO[B] = f(internal)

    def map[B](f: A => B): IO[B] = flatMap(x => IO(f(x)))

    def get: A = internal
}

object IO {
    def apply[A](value: => A): IO[A] = new IO(value)
}
