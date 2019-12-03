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

// This "Do It Yourself Floating Point" class implements a floating-point number
// with a uint64 significand and an int exponent. Normalized DiyFp numbers will
// have the most significant bit of the significand set.
// Multiplication and Subtraction do not normalize their results.
// DiyFp are not designed to contain special doubles (NaN and Infinity).
private object DiyFp {
  final val kSignificandSize = 64
  final val kUint64MSB = ULong(1 << 63)

  def normalize(a: DiyFp): DiyFp = {
    val result = new DiyFp(a.f, a.e)
    result.normalize()
    result
  }
}

private class DiyFp(var f : ULong, var e : Int) {
  import DiyFp._

  // this = this - other.
  // The exponents of both numbers must be the same and the significand of this
  // must be bigger than the significand of other.
  // The result will not be normalized.
  def -=(other: DiyFp): Unit = {
    assume(e == other.e)
    assume(f >= other.f)
    f -= other.f
  }

  // Returns a - b.
  // The exponents of both numbers must be the same and this must be bigger
  // than other. The result will not be normalized.
  def -(b: DiyFp): DiyFp = {
    val result = new DiyFp(f, e)
    result -= b
    result
  }


  // this = this * other.
  def *=(other: DiyFp): Unit = {
    // Simply "emulates" a 128 bit multiplication.
    // However: the resulting number only contains 64 bits. The least
    // significant 64 bits are only used for rounding the most significant 64
    // bits.
    val kM32 = ULong(0xFFFFFFFFL)
    val a = f >> 32
    val b = f & kM32
    val c = other.f >> 32
    val d = other.f & kM32
    val ac = a * c
    val bc = b * c
    val ad = a * d
    val bd = b * d
    var tmp = (bd >> 32) + (ad & kM32) + (bc & kM32)
    // By adding 1U << 31 to tmp we round the final result.
    // Halfway cases will be round up.
    tmp += ULong(1L << 31)
    val result_f = ac + (ad >> 32) + (bc >> 32) + (tmp >> 32)
    e += other.e + 64
    f = result_f
  }

  def *(b: DiyFp): DiyFp = {
    val result = new DiyFp(f, e)
    result *= b
    result
  }

  def normalize(): Unit = {
    var f = this.f
    var e = this.e
    assume(f != ULong(0))

    // This method is mainly called for normalizing boundaries. In general
    // boundaries need to be shifted by 10 bits. We thus optimize for this case.
    val k10MSBits = ULong(0xFFC0L << 48)
    while((f & k10MSBits) == ULong(0)) {
      f <<= 10
      e -= 10
    }
    while((f & kUint64MSB) == ULong(0)) {
      f <<= 1
      e -= 1
    }
    this.f = f
    this.e = e
  }
}
