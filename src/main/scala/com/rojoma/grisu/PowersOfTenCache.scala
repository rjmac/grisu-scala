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

private object PowersOfTenCache {
  // Not all powers of ten are cached. The decimal exponent of two neighboring
  // cached numbers will differ by kDecimalExponentDistance.
  private final val kDecimalExponentDistance = 8

  val kMinDecimalExponent = -348
  val kMaxDecimalExponent = 340

  case class ResultBox(power: DiyFp, decimal_exponent: Int)

  // Returns a cached power-of-ten with a binary exponent in the range
  // [min_exponent; max_exponent] (boundaries included).
  def cachedPowerForBinaryExponentRange(min_exponent: Int,
                                        max_exponent: Int): ResultBox =
  {
    val kQ = DiyFp.kSignificandSize
    val k = Math.ceil((min_exponent + kQ - 1) * kD_1_LOG2_10)
    val foo = kCachedPowersOffset
    val index = (foo + k.toInt - 1) / kDecimalExponentDistance + 1
    require(0 <= index && index < kCachedPowers.length)
    val cached_power = kCachedPowers(index)
    assert(min_exponent <= cached_power.binary_exponent)
    assert(cached_power.binary_exponent <= max_exponent)
    ResultBox(new DiyFp(cached_power.significand, cached_power.binary_exponent),
              cached_power.decimal_exponent)
  }

  // Returns a cached power of ten x ~= 10^k such that
  //   k <= decimal_exponent < k + kCachedPowersDecimalDistance.
  // The given decimal_exponent must satisfy
  //   kMinDecimalExponent <= requested_exponent, and
  //   requested_exponent < kMaxDecimalExponent + kDecimalExponentDistance.
  def cachedPowerForDecimalExponent(requested_exponent: Int): ResultBox = {
    require(kMinDecimalExponent <= requested_exponent)
    require(requested_exponent < kMaxDecimalExponent + kDecimalExponentDistance)
    val index = (requested_exponent + kCachedPowersOffset) / kDecimalExponentDistance
    val cached_power = kCachedPowers(index)
    val power = new DiyFp(cached_power.significand, cached_power.binary_exponent)
    val found_exponent = cached_power.decimal_exponent
    assert(found_exponent <= requested_exponent)
    assert(requested_exponent < found_exponent + kDecimalExponentDistance)
    ResultBox(power, found_exponent)
  }

  private class CachedPower(msb: Int, lsbs : Long, binExp : Int, decExp : Int) {
    val significand = ULong((msb.toLong << 56) | lsbs)
    val binary_exponent = binExp.toShort
    val decimal_exponent = decExp.toShort
  }

  private val kCachedPowers = Array[CachedPower](
      new CachedPower(msb = 0xfa, lsbs = 0x8fd5a0081c0288L, binExp = -1220, decExp = -348),
      new CachedPower(msb = 0xba, lsbs = 0xaee17fa23ebf76L, binExp = -1193, decExp = -340),
      new CachedPower(msb = 0x8b, lsbs = 0x16fb203055ac76L, binExp = -1166, decExp = -332),
      new CachedPower(msb = 0xcf, lsbs = 0x42894a5dce35eaL, binExp = -1140, decExp = -324),
      new CachedPower(msb = 0x9a, lsbs = 0x6bb0aa55653b2dL, binExp = -1113, decExp = -316),
      new CachedPower(msb = 0xe6, lsbs = 0x1acf033d1a45dfL, binExp = -1087, decExp = -308),
      new CachedPower(msb = 0xab, lsbs = 0x70fe17c79ac6caL, binExp = -1060, decExp = -300),
      new CachedPower(msb = 0xff, lsbs = 0x77b1fcbebcdc4fL, binExp = -1034, decExp = -292),
      new CachedPower(msb = 0xbe, lsbs = 0x5691ef416bd60cL, binExp = -1007, decExp = -284),
      new CachedPower(msb = 0x8d, lsbs = 0xd01fad907ffc3cL, binExp = -980, decExp = -276),
      new CachedPower(msb = 0xd3, lsbs = 0x515c2831559a83L, binExp = -954, decExp = -268),
      new CachedPower(msb = 0x9d, lsbs = 0x71ac8fada6c9b5L, binExp = -927, decExp = -260),
      new CachedPower(msb = 0xea, lsbs = 0x9c227723ee8bcbL, binExp = -901, decExp = -252),
      new CachedPower(msb = 0xae, lsbs = 0xcc49914078536dL, binExp = -874, decExp = -244),
      new CachedPower(msb = 0x82, lsbs = 0x3c12795db6ce57L, binExp = -847, decExp = -236),
      new CachedPower(msb = 0xc2, lsbs = 0x1094364dfb5637L, binExp = -821, decExp = -228),
      new CachedPower(msb = 0x90, lsbs = 0x96ea6f3848984fL, binExp = -794, decExp = -220),
      new CachedPower(msb = 0xd7, lsbs = 0x7485cb25823ac7L, binExp = -768, decExp = -212),
      new CachedPower(msb = 0xa0, lsbs = 0x86cfcd97bf97f4L, binExp = -741, decExp = -204),
      new CachedPower(msb = 0xef, lsbs = 0x340a98172aace5L, binExp = -715, decExp = -196),
      new CachedPower(msb = 0xb2, lsbs = 0x3867fb2a35b28eL, binExp = -688, decExp = -188),
      new CachedPower(msb = 0x84, lsbs = 0xc8d4dfd2c63f3bL, binExp = -661, decExp = -180),
      new CachedPower(msb = 0xc5, lsbs = 0xdd44271ad3cdbaL, binExp = -635, decExp = -172),
      new CachedPower(msb = 0x93, lsbs = 0x6b9fcebb25c996L, binExp = -608, decExp = -164),
      new CachedPower(msb = 0xdb, lsbs = 0xac6c247d62a584L, binExp = -582, decExp = -156),
      new CachedPower(msb = 0xa3, lsbs = 0xab66580d5fdaf6L, binExp = -555, decExp = -148),
      new CachedPower(msb = 0xf3, lsbs = 0xe2f893dec3f126L, binExp = -529, decExp = -140),
      new CachedPower(msb = 0xb5, lsbs = 0xb5ada8aaff80b8L, binExp = -502, decExp = -132),
      new CachedPower(msb = 0x87, lsbs = 0x625f056c7c4a8bL, binExp = -475, decExp = -124),
      new CachedPower(msb = 0xc9, lsbs = 0xbcff6034c13053L, binExp = -449, decExp = -116),
      new CachedPower(msb = 0x96, lsbs = 0x4e858c91ba2655L, binExp = -422, decExp = -108),
      new CachedPower(msb = 0xdf, lsbs = 0xf9772470297ebdL, binExp = -396, decExp = -100),
      new CachedPower(msb = 0xa6, lsbs = 0xdfbd9fb8e5b88fL, binExp = -369, decExp = -92),
      new CachedPower(msb = 0xf8, lsbs = 0xa95fcf88747d94L, binExp = -343, decExp = -84),
      new CachedPower(msb = 0xb9, lsbs = 0x4470938fa89bcfL, binExp = -316, decExp = -76),
      new CachedPower(msb = 0x8a, lsbs = 0x08f0f8bf0f156bL, binExp = -289, decExp = -68),
      new CachedPower(msb = 0xcd, lsbs = 0xb02555653131b6L, binExp = -263, decExp = -60),
      new CachedPower(msb = 0x99, lsbs = 0x3fe2c6d07b7facL, binExp = -236, decExp = -52),
      new CachedPower(msb = 0xe4, lsbs = 0x5c10c42a2b3b06L, binExp = -210, decExp = -44),
      new CachedPower(msb = 0xaa, lsbs = 0x242499697392d3L, binExp = -183, decExp = -36),
      new CachedPower(msb = 0xfd, lsbs = 0x87b5f28300ca0eL, binExp = -157, decExp = -28),
      new CachedPower(msb = 0xbc, lsbs = 0xe5086492111aebL, binExp = -130, decExp = -20),
      new CachedPower(msb = 0x8c, lsbs = 0xbccc096f5088ccL, binExp = -103, decExp = -12),
      new CachedPower(msb = 0xd1, lsbs = 0xb71758e219652cL, binExp = -77, decExp = -4),
      new CachedPower(msb = 0x9c, lsbs = 0x40000000000000L, binExp = -50, decExp = 4),
      new CachedPower(msb = 0xe8, lsbs = 0xd4a51000000000L, binExp = -24, decExp = 12),
      new CachedPower(msb = 0xad, lsbs = 0x78ebc5ac620000L, binExp = 3, decExp = 20),
      new CachedPower(msb = 0x81, lsbs = 0x3f3978f8940984L, binExp = 30, decExp = 28),
      new CachedPower(msb = 0xc0, lsbs = 0x97ce7bc90715b3L, binExp = 56, decExp = 36),
      new CachedPower(msb = 0x8f, lsbs = 0x7e32ce7bea5c70L, binExp = 83, decExp = 44),
      new CachedPower(msb = 0xd5, lsbs = 0xd238a4abe98068L, binExp = 109, decExp = 52),
      new CachedPower(msb = 0x9f, lsbs = 0x4f2726179a2245L, binExp = 136, decExp = 60),
      new CachedPower(msb = 0xed, lsbs = 0x63a231d4c4fb27L, binExp = 162, decExp = 68),
      new CachedPower(msb = 0xb0, lsbs = 0xde65388cc8ada8L, binExp = 189, decExp = 76),
      new CachedPower(msb = 0x83, lsbs = 0xc7088e1aab65dbL, binExp = 216, decExp = 84),
      new CachedPower(msb = 0xc4, lsbs = 0x5d1df942711d9aL, binExp = 242, decExp = 92),
      new CachedPower(msb = 0x92, lsbs = 0x4d692ca61be758L, binExp = 269, decExp = 100),
      new CachedPower(msb = 0xda, lsbs = 0x01ee641a708deaL, binExp = 295, decExp = 108),
      new CachedPower(msb = 0xa2, lsbs = 0x6da3999aef774aL, binExp = 322, decExp = 116),
      new CachedPower(msb = 0xf2, lsbs = 0x09787bb47d6b85L, binExp = 348, decExp = 124),
      new CachedPower(msb = 0xb4, lsbs = 0x54e4a179dd1877L, binExp = 375, decExp = 132),
      new CachedPower(msb = 0x86, lsbs = 0x5b86925b9bc5c2L, binExp = 402, decExp = 140),
      new CachedPower(msb = 0xc8, lsbs = 0x3553c5c8965d3dL, binExp = 428, decExp = 148),
      new CachedPower(msb = 0x95, lsbs = 0x2ab45cfa97a0b3L, binExp = 455, decExp = 156),
      new CachedPower(msb = 0xde, lsbs = 0x469fbd99a05fe3L, binExp = 481, decExp = 164),
      new CachedPower(msb = 0xa5, lsbs = 0x9bc234db398c25L, binExp = 508, decExp = 172),
      new CachedPower(msb = 0xf6, lsbs = 0xc69a72a3989f5cL, binExp = 534, decExp = 180),
      new CachedPower(msb = 0xb7, lsbs = 0xdcbf5354e9beceL, binExp = 561, decExp = 188),
      new CachedPower(msb = 0x88, lsbs = 0xfcf317f22241e2L, binExp = 588, decExp = 196),
      new CachedPower(msb = 0xcc, lsbs = 0x20ce9bd35c78a5L, binExp = 614, decExp = 204),
      new CachedPower(msb = 0x98, lsbs = 0x165af37b2153dfL, binExp = 641, decExp = 212),
      new CachedPower(msb = 0xe2, lsbs = 0xa0b5dc971f303aL, binExp = 667, decExp = 220),
      new CachedPower(msb = 0xa8, lsbs = 0xd9d1535ce3b396L, binExp = 694, decExp = 228),
      new CachedPower(msb = 0xfb, lsbs = 0x9b7cd9a4a7443cL, binExp = 720, decExp = 236),
      new CachedPower(msb = 0xbb, lsbs = 0x764c4ca7a44410L, binExp = 747, decExp = 244),
      new CachedPower(msb = 0x8b, lsbs = 0xab8eefb6409c1aL, binExp = 774, decExp = 252),
      new CachedPower(msb = 0xd0, lsbs = 0x1fef10a657842cL, binExp = 800, decExp = 260),
      new CachedPower(msb = 0x9b, lsbs = 0x10a4e5e9913129L, binExp = 827, decExp = 268),
      new CachedPower(msb = 0xe7, lsbs = 0x109bfba19c0c9dL, binExp = 853, decExp = 276),
      new CachedPower(msb = 0xac, lsbs = 0x2820d9623bf429L, binExp = 880, decExp = 284),
      new CachedPower(msb = 0x80, lsbs = 0x444b5e7aa7cf85L, binExp = 907, decExp = 292),
      new CachedPower(msb = 0xbf, lsbs = 0x21e44003acdd2dL, binExp = 933, decExp = 300),
      new CachedPower(msb = 0x8e, lsbs = 0x679c2f5e44ff8fL, binExp = 960, decExp = 308),
      new CachedPower(msb = 0xd4, lsbs = 0x33179d9c8cb841L, binExp = 986, decExp = 316),
      new CachedPower(msb = 0x9e, lsbs = 0x19db92b4e31ba9L, binExp = 1013, decExp = 324),
      new CachedPower(msb = 0xeb, lsbs = 0x96bf6ebadf77d9L, binExp = 1039, decExp = 332),
      new CachedPower(msb = 0xaf, lsbs = 0x87023b9bf0ee6bL, binExp = 1066, decExp = 340)
    )

  final val kCachedPowersOffset = 348;  // -1 * the first decimal_exponent.
  final val kD_1_LOG2_10 = 0.30102999566398114;  //  1 / lg(10)
}
