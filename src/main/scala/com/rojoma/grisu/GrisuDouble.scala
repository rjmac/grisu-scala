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

import spire.math.ULong

private object GrisuDouble {
  final val kSignMask = ULong(1L << 63)
  final val kExponentMask = ULong(0x7FF0000000000000L)
  final val kSignificandMask = ULong(0x000FFFFFFFFFFFFFL)
  final val kHiddenBit = ULong(0x0010000000000000L)
  final val kPhysicalSignificandSize = 52  // Excludes the hidden bit.
  final val kSignificandSize = 53

  final val kExponentBias = 0x3FF + kPhysicalSignificandSize
  final val kDenormalExponent = -kExponentBias + 1
  final val kMaxExponent = 0x7FF - kExponentBias
  final val kInfinity = ULong(0x7FF0000000000000L)


  // Returns the significand size for a given order of magnitude.
  // If v = f*2^e with 2^p-1 <= f <= 2^p then p+e is v's order of magnitude.
  // This function returns the number of significant binary digits v will have
  // once it's encoded into a double. In almost all cases this is equal to
  // kSignificandSize. The only exceptions are denormals. They start with
  // leading zeroes and their effective significand-size is hence smaller.
  def significandSizeForOrderOfMagnitude(order: Int): Int = {
    if(order >= (kDenormalExponent + kSignificandSize)) {
      return kSignificandSize;
    }
    if (order <= kDenormalExponent) 0
    else order - kDenormalExponent;
  }

  val Infinity = Double.PositiveInfinity;

  val NaN = Double.NaN

  def diyFpToULong(diy_fp: DiyFp): ULong = {
    var significand = diy_fp.f;
    var exponent = diy_fp.e;
    while(significand > kHiddenBit + kSignificandMask) {
      significand >>= 1;
      exponent += 1;
    }

    if(exponent >= kMaxExponent) return kInfinity;
    if(exponent < kDenormalExponent) return ULong(0);

    while (exponent > kDenormalExponent && (significand & kHiddenBit) == ULong(0)) {
      significand <<= 1;
      exponent -= 1;
    }

    val biased_exponent =
      if (exponent == kDenormalExponent && (significand & kHiddenBit) == ULong(0)) {
        ULong(0L)
      } else {
        ULong(exponent + kExponentBias)
      }
    (significand & kSignificandMask) | (biased_exponent << kPhysicalSignificandSize);
  }
}

private class GrisuDouble(private var value: Double, private var d64: ULong) {
  import GrisuDouble._

  def this(d: Double) = this(d, ULong(java.lang.Double.doubleToRawLongBits(d)))

  def this(d64: ULong) = this(java.lang.Double.longBitsToDouble(d64.toLong), d64)

  def this(diy_fp: DiyFp) = this(GrisuDouble.diyFpToULong(diy_fp))

  def toDouble = value

  // The value encoded by this Double must be greater or equal to +0.0.
  // It must not be special (infinity, or NaN).
  def asDiyFp: DiyFp = {
    assume(sign > 0, "sign > 0")
    assume(!isSpecial, "!isSpecial")
    new DiyFp(significand, exponent);
  }

  // The value encoded by this Double must be strictly greater than 0.
  def asNormalizedDiyFp: DiyFp = {
    assume(value > 0.0, "value > 0.0")

    val d64 = this.d64
    var f = ULong(0)
    var e = 0

    if(isDenormal) {
      f = d64 & kSignificandMask;
      e = kDenormalExponent;
    } else {
      f = (d64 & kSignificandMask) + kHiddenBit;
      e = ((d64 & kExponentMask) >> kPhysicalSignificandSize).toInt - kExponentBias;
    }

    // The current double could be a denormal.
    while((f & kHiddenBit) == ULong(0)) {
      f <<= 1;
      e -= 1;
    }
    // Do the final shifts in one go.
    f <<= DiyFp.kSignificandSize - kSignificandSize;
    e -= DiyFp.kSignificandSize - kSignificandSize;
    new DiyFp(f, e);
  }

  // Returns the double's bit as UInt64.
  def asULong: ULong = d64

  def exponent: Int = {
    if(isDenormal) return kDenormalExponent

    val biased_e = ((d64 & kExponentMask) >> kPhysicalSignificandSize).toInt
    biased_e - kExponentBias
  }

  def significand: ULong = {
    val significand = d64 & kSignificandMask;
    if(isDenormal) return significand;
    significand + kHiddenBit;
  }

  // Returns true if the double is a denormal.
  def isDenormal: Boolean = (d64 & kExponentMask) == ULong(0);

  // We consider denormals not to be special.
  // Hence only Infinity and NaN are special.
  def isSpecial: Boolean = (d64 & kExponentMask) == kExponentMask;

  def isNaN: Boolean =
    ((d64 & kExponentMask) == kExponentMask) &&
      ((d64 & kSignificandMask) != ULong(0));

  def isInfinite: Boolean =
    ((d64 & kExponentMask) == kExponentMask) &&
      ((d64 & kSignificandMask) == ULong(0));

  def sign: Int = if((d64 & kSignMask) == ULong(0)) 1 else -1;

  // Precondition: the value encoded by this Double must be greater or equal
  // than +0.0.
  def upperBoundary: DiyFp = {
    assume(sign > 0, "sign > 0");
    new DiyFp(significand * ULong(2) + ULong(1), exponent - 1);
  }

  // Computes the two boundaries of this.
  // The bigger boundary (m_plus) is normalized. The lower boundary has the same
  // exponent as m_plus.
  // Precondition: the value encoded by this Double must be greater than 0.
  def normalizedBoundaries: (DiyFp, DiyFp) = {
    assume(value > 0.0, "value > 0.0");

    val d64 = this.d64;
    var vF = ULong(0);
    var vE = 0;
    if(isDenormal) {
      vF = d64 & kSignificandMask;
      vE = kDenormalExponent;
    } else {
      vF = (d64 & kSignificandMask) + kHiddenBit;
      vE = ((d64 & kExponentMask) >> kPhysicalSignificandSize).toInt - kExponentBias;
    }

    var plusF = (vF << 1) + ULong(1);
    var plusE = vE - 1;

    // This code is manually inlined from the GrisuDouble.Normalize() method,
    // because the .NET JIT (at least the 64-bit one as of version 4) is too
    // incompetent to do it itself.
    val k10MSBits = ULong(0xFFC0L << 48);
    val kUint64MSB = ULong(1L << 63)
    while((plusF & k10MSBits) == ULong(0)) {
      plusF <<= 10;
      plusE -= 10;
    }
    while((plusF & kUint64MSB) == ULong(0)) {
      plusF <<= 1;
      plusE -= 1;
    }

    var minusF = ULong(0);
    var minusE = 0

    val significand_is_zero = (vF == kHiddenBit)
    if (significand_is_zero && vE != kDenormalExponent) {
      // The boundary is closer. Think of v = 1000e10 and v- = 9999e9.
      // Then the boundary (== (v - v-)/2) is not just at a distance of 1e9 but
      // at a distance of 1e8.
      // The only exception is for the smallest normal: the largest denormal is
      // at the same distance as its successor.
      // Note: denormals have the same exponent as the smallest normals.
      minusF = (vF << 2) - ULong(1);
      minusE = vE - 2;
    } else {
      minusF = (vF << 1) - ULong(1);
      minusE = vE - 1;
    }

    (new DiyFp(minusF << (minusE - plusE), plusE), new DiyFp(plusF, plusE))
  }
}
