// Copyright 2010 the V8 project authors. All rights reserved.
// Copyright 2011-2012, Kevin Ring. All rights reserved.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Google Inc. nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

package com.rojoma.grisu

import java.io.StringWriter
import org.scalatest.{FunSuite, MustMatchers}
import spire.syntax.cfor._
import scala.util.Random

class GrisuTests extends FunSuite with MustMatchers {
  private class Stopwatch {
    private var a = 0L
    private var b = 0L
    def start() { a = System.currentTimeMillis() }
    def stop() { b = System.currentTimeMillis() }
    def elapsedMilliseconds = b - a
  }

  test("performance") {
    doPerf();
    doPerf();
    doPerf();
  }

  private def doPerf() {
    val r = new Random(1);
    val values = Array.fill(1000000) {
        (r.nextDouble() - 0.5) * Math.pow(10, r.nextDouble() * 308);
      }

    var w = new StringWriter

    val sw = new Stopwatch();
    sw.start();

    cfor(0)(_ < values.length, _ + 1) { i =>
      w.write(values(i).toString)
    }
    sw.stop();

    println("builtin length: " + w.toString.length);
    println("builtin time: " + sw.elapsedMilliseconds);
    if (values.length < 100)
      println(w.toString);

    w = new StringWriter

    sw.start();
    cfor(0)(_ < values.length, _ + 1) { i =>
      try {
        Grisu.toWriter(w, values(i));
      } catch {
        case e: AssertionError =>
          println(i + " - " + values(i))
          throw e
      }
    }
    sw.stop();
    println("grisu length: " + w.toString.length);
    println("grisu time: " + sw.elapsedMilliseconds);

    if (values.length < 100)
      println(w.toString);
  }

  test("doubleToString") {
    checkDoubleToStringEquals("0", 0.0);
    checkDoubleToStringEquals("12345", 12345.0);
    checkDoubleToStringEquals("1.2345e27", 12345e23);
    checkDoubleToStringEquals("1e21", 1e21);
    checkDoubleToStringEquals("1e20", 1e20);
    checkDoubleToStringEquals("1.1111111111111111e20", 111111111111111111111.0);
    checkDoubleToStringEquals("1.1111111111111111e21", 1111111111111111111111.0);
    checkDoubleToStringEquals("1.1111111111111111e22", 11111111111111111111111.0);
    checkDoubleToStringEquals("-1e-5", -0.00001);
    checkDoubleToStringEquals("-1e-6", -0.000001);
    checkDoubleToStringEquals("-1e-7", -0.0000001);
    checkDoubleToStringEquals("0", -0.0);
    checkDoubleToStringEquals("0.1", 0.1);
    checkDoubleToStringEquals("0.01", 0.01);
    checkDoubleToStringEquals("1", 1.0);
    checkDoubleToStringEquals("10", 10.0);
    checkDoubleToStringEquals("1100", 1100.0);
    checkDoubleToStringEquals("1122", 1122.0);
    checkDoubleToStringEquals("1e4", 10000.0);
    checkDoubleToStringEquals("11100", 11100.0);
    checkDoubleToStringEquals("1e5", 100000.0);
    checkDoubleToStringEquals("1e-6", 0.000001);
    checkDoubleToStringEquals("1e-7", 0.0000001);
    checkDoubleToStringEquals("1e20", 100000000000000000000.0);
    checkDoubleToStringEquals("Infinity", Double.PositiveInfinity);
    checkDoubleToStringEquals("-Infinity", Double.NegativeInfinity);
    checkDoubleToStringEquals("NaN", Double.NaN);
    checkDoubleToStringEquals("NaN", -Double.NaN);
    checkDoubleToStringEquals("3.5844466002796428E298", 3.5844466002796428e+298);
  }

  def checkDoubleToStringEquals(expected: String, value: Double) {
    try {
      Grisu.toString(value) must equal (expected)
    } catch {
      case e: AssertionError =>
        e.printStackTrace()
        fail(expected)
    }
  }
}
