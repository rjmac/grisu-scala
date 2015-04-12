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

import java.io.{StringWriter, Writer}

import spire.math.ULong
import spire.math.UInt

object M extends App {
  println(Grisu.toString(1.37988403153109808E17))
}

object Grisu {
  private val ts_decimal_rep = new ThreadLocal[Array[Char]] {
      override protected def initialValue = new Array[Char](kBase10MaximalLength + 1)
    }

  private class IntBoxes {
    var a: Int = _
    var b: Int = _
  }

  def toString(value: Double): String = {
    val sw = new StringWriter
    toString(value, sw)
    sw.toString
  }

  def toString(value: Double, writer: Writer) {
    if (value < 0.0) {
      writer.write('-');
      return toString(-value, writer)
    }

    val grisuDouble = new GrisuDouble(value);
    if (grisuDouble.isSpecial) {
      return handleSpecialValues(grisuDouble, writer);
    }

    val decimal_rep = ts_decimal_rep.get();

    val intBoxes = new IntBoxes
    if (!doubleToShortestAscii(grisuDouble, decimal_rep, intBoxes)) {
      writer.write(value.toString)
      return;
    }
    val decimal_rep_length = intBoxes.a
    val decimal_point = intBoxes.b

    var decimalRepLength = decimal_rep_length;
    if (decimal_point < 1) {
      decimalRepLength += -decimal_point + 1;
    } else if (decimal_point >= decimal_rep_length) {
      decimalRepLength += decimal_point - decimal_rep_length + 1;
    }

    val exponent = decimal_point - 1;
    val absExponent = Math.abs(exponent);
    var exponentRepLength = decimal_rep_length + 3;
    if (exponent < 0) exponentRepLength += 1;
    if (absExponent >= 10) {
      exponentRepLength += 1;
      if (absExponent >= 100) exponentRepLength += 1;
    }

    if (decimalRepLength <= exponentRepLength) {
      createDecimalRepresentation(decimal_rep, decimal_rep_length,
                                  decimal_point,
                                  Math.max(0, decimal_rep_length - decimal_point),
                                  writer);
    } else {
      createExponentialRepresentation(decimal_rep, decimal_rep_length, exponent,
                                      writer);
    }
  }

  // The maximal number of digits that are needed to emit a double in base 10.
  // A higher precision can be achieved by using more digits, but the shortest
  // accurate representation of any double will never use more digits than
  // kBase10MaximalLength.
  // Note that DoubleToAscii null-terminates its input. So the given buffer
  // should be at least kBase10MaximalLength + 1 characters long.
  final val kBase10MaximalLength = 17;

  final val infinity_symbol = "Infinity";
  final val nan_symbol = "NaN";
  final val exponent_character = 'e';

  private def handleSpecialValues(double_inspect: GrisuDouble,
                                  writer: Writer): Unit =
  {
    if(double_inspect.isInfinite) {
      if(double_inspect.toDouble < 0) {
        writer.write('-');
      }
      writer.write(infinity_symbol);
      return;
    }
    if(double_inspect.isNaN) {
      writer.write(nan_symbol);
      return;
    }
  }

  // intBoxes is (length, point)
  private def doubleToShortestAscii(v: GrisuDouble, buffer: Array[Char], intBoxes: IntBoxes): Boolean = {
    assert(!v.isSpecial);
    assert(v.toDouble >= 0.0);

    val value = v.toDouble;

    if (value == 0.0) {
      buffer(0) = '0';
      buffer(1) = 0.toChar;
      intBoxes.a = 1;
      intBoxes.b = 1;
      return true;
    }

    val result = Grisu3(v, buffer, intBoxes);
    if (result) intBoxes.b += intBoxes.a
    else intBoxes.b = 0;
    return result;
  }

  // The minimal and maximal target exponent define the range of w's binary
  // exponent, where 'w' is the result of multiplying the input by a cached power
  // of ten.
  //
  // A different range might be chosen on a different platform, to optimize digit
  // generation, but a smaller range requires more powers of ten to be cached.
  final val kMinimalTargetExponent = -60;
  final val kMaximalTargetExponent = -32;

  // Provides a decimal representation of v.
  // Returns true if it succeeds, otherwise the result cannot be trusted.
  // There will be *length digits inside the buffer (not null-terminated).
  // If the function returns true then
  //        v == (double) (buffer * 10^decimal_exponent).
  // The digits in the buffer are the shortest representation possible: no
  // 0.09999999999999999 instead of 0.1. The shorter representation will even be
  // chosen even if the longer one would be closer to v.
  // The last digit will be closest to the actual v. That is, even if several
  // digits might correctly yield 'v' when read again, the closest will be
  // computed.
  private def Grisu3(v: GrisuDouble,
                     buffer: Array[Char],
                     intBoxes: IntBoxes): Boolean =
  {
    val w = v.asNormalizedDiyFp;
    // boundary_minus and boundary_plus are the boundaries between v and its
    // closest floating-point neighbors. Any number strictly between
    // boundary_minus and boundary_plus will round to v when convert to a double.
    // Grisu3 will never output representations that lie exactly on a boundary.
    val (boundary_minus, boundary_plus) = v.normalizedBoundaries;
    assert(boundary_plus.e == w.e);
    val ten_mk_minimal_binary_exponent =
      kMinimalTargetExponent - (w.e + DiyFp.kSignificandSize);
    val ten_mk_maximal_binary_exponent =
      kMaximalTargetExponent - (w.e + DiyFp.kSignificandSize);
    val PowersOfTenCache.ResultBox(ten_mk, mk) = PowersOfTenCache.cachedPowerForBinaryExponentRange(
        ten_mk_minimal_binary_exponent,
        ten_mk_maximal_binary_exponent);
    assert((kMinimalTargetExponent <= w.e + ten_mk.e +
              DiyFp.kSignificandSize) &&
             (kMaximalTargetExponent >= w.e + ten_mk.e +
                DiyFp.kSignificandSize));

    // Note that ten_mk is only an approximation of 10^-k. A DiyFp only contains a
    // 64 bit significand and ten_mk is thus only precise up to 64 bits.

    // The DiyFp.Times procedure rounds its result, and ten_mk is approximated
    // too. The variable scaled_w (as well as scaled_boundary_minus/plus) are now
    // off by a small amount.
    // In fact: scaled_w - w*10^k < 1ulp (unit in the last place) of scaled_w.
    // In other words: let f = scaled_w.f() and e = scaled_w.e(), then
    //           (f-1) * 2^e < w*10^k < (f+1) * 2^e
    //DiyFp scaled_w = DiyFp.Times(ref w, ref ten_mk);
    w *= ten_mk
    assert(w.e == boundary_plus.e + ten_mk.e + DiyFp.kSignificandSize);
    // In theory it would be possible to avoid some recomputations by computing
    // the difference between w and boundary_minus/plus (a power of 2) and to
    // compute scaled_boundary_minus/plus by subtracting/adding from
    // scaled_w. However the code becomes much less readable and the speed
    // enhancements are not terriffic.
    //DiyFp scaled_boundary_minus = DiyFp.Times(ref boundary_minus, ref ten_mk);
    boundary_minus *= ten_mk;
    //DiyFp scaled_boundary_plus = DiyFp.Times(ref boundary_plus, ref ten_mk);
    boundary_plus *= ten_mk;

    // DigitGen will generate the digits of scaled_w. Therefore we have
    // v == (double) (scaled_w * 10^-mk).
    // Set decimal_exponent == -mk and pass it to DigitGen. If scaled_w is not an
    // integer than it will be updated. For instance if scaled_w == 1.23 then
    // the buffer will be filled with "123" und the decimal_exponent will be
    // decreased by 2.
    val result = digitGen(boundary_minus, w, boundary_plus,
                          buffer, intBoxes);
    intBoxes.b = -mk + intBoxes.b;
    return result;
  }

  // Generates the digits of input number w.
  // w is a floating-point number (DiyFp), consisting of a significand and an
  // exponent. Its exponent is bounded by kMinimalTargetExponent and
  // kMaximalTargetExponent.
  //       Hence -60 <= w.e() <= -32.
  //
  // Returns false if it fails, in which case the generated digits in the buffer
  // should not be used.
  // Preconditions:
  //  * low, w and high are correct up to 1 ulp (unit in the last place). That
  //    is, their error must be less than a unit of their last digits.
  //  * low.e() == w.e() == high.e()
  //  * low < w < high, and taking into account their error: low~ <= high~
  //  * kMinimalTargetExponent <= w.e() <= kMaximalTargetExponent
  // Postconditions: returns false if procedure fails.
  //   otherwise:
  //     * buffer is not null-terminated, but len contains the number of digits.
  //     * buffer contains the shortest possible decimal digit-sequence
  //       such that LOW < buffer * 10^kappa < HIGH, where LOW and HIGH are the
  //       correct values of low and high (without their error).
  //     * if more than one decimal representation gives the minimal number of
  //       decimal digits then the one closest to W (where W is the correct value
  //       of w) is chosen.
  // Remark: this procedure takes into account the imprecision of its input
  //   numbers. If the precision is not enough to guarantee all the postconditions
  //   then false is returned. This usually happens rarely (~0.5%).
  //
  // Say, for the sake of example, that
  //   w.e() == -48, and w.f() == 0x1234567890abcdef
  // w's value can be computed by w.f() * 2^w.e()
  // We can obtain w's integral digits by simply shifting w.f() by -w.e().
  //  -> w's integral part is 0x1234
  //  w's fractional part is therefore 0x567890abcdef.
  // Printing w's integral part is easy (simply print 0x1234 in decimal).
  // In order to print its fraction we repeatedly multiply the fraction by 10 and
  // get each digit. Example the first digit after the point would be computed by
  //   (0x567890abcdef * 10) >> 48. -> 3
  // The whole thing becomes slightly more complicated because we want to stop
  // once we have enough digits. That is, once the digits inside the buffer
  // represent 'w' we can stop. Everything inside the interval low - high
  // represents w. However we have to pay attention to low, high and w's
  // imprecision.
  private def digitGen(low: DiyFp,
                       w: DiyFp,
                       high: DiyFp,
                       buffer: Array[Char],
                       intBoxes: IntBoxes): Boolean = // intBoxes is (length, kappa)
  {
    assert(low.e == w.e && w.e == high.e);
    assert(low.f + ULong(1) <= high.f - ULong(1));
    assert(kMinimalTargetExponent <= w.e && w.e <= kMaximalTargetExponent);
    // low, w and high are imprecise, but by less than one ulp (unit in the last
    // place).
    // If we remove (resp. add) 1 ulp from low (resp. high) we are certain that
    // the new numbers are outside of the interval we want the final
    // representation to lie in.
    // Inversely adding (resp. removing) 1 ulp from low (resp. high) would yield
    // numbers that are certain to lie in the interval. We will use this fact
    // later on.
    // We will now start by generating the digits within the uncertain
    // interval. Later we will weed out representations that lie outside the safe
    // interval and thus _might_ lie outside the correct interval.
    var unit = ULong(1L);
    val too_low = new DiyFp(low.f - unit, low.e);
    val too_high = new DiyFp(high.f + unit, high.e);
    // too_low and too_high are guaranteed to lie outside the interval we want the
    // generated number in.
    val unsafe_interval = too_high - too_low
    // We now cut the input number into two parts: the integral digits and the
    // fractionals. We will not write any decimal separator though, but adapt
    // kappa instead.
    // Reminder: we are currently computing the digits (stored inside the buffer)
    // such that:   too_low < buffer * 10^kappa < too_high
    // We use too_high for the digit_generation and stop as soon as possible.
    // If we stop early we effectively round down.
    val one = new DiyFp(ULong(1L) << -w.e, w.e);
    // Division by one is a shift.
    var integrals = UInt((too_high.f >> -one.e).toInt);
    // Modulo by one is an and.
    var fractionals = too_high.f & (one.f - ULong(1));

    biggestPowerTen(integrals, DiyFp.kSignificandSize - (-one.e), intBoxes)
    var divisor = UInt(intBoxes.a)
    var kappa = intBoxes.b
    var length = 0

    // Loop invariant: buffer = too_high / 10^kappa  (integer division)
    // The invariant holds for the first iteration: kappa has been initialized
    // with the divisor exponent + 1. And the divisor is the biggest power of ten
    // that is smaller than integrals.
    var unsafeIntervalF = unsafe_interval.f;
    while(kappa > 0) {
      val digit = (integrals / divisor).toInt;
      buffer(length) = ('0' + digit).toChar;
      length += 1;
      integrals %= divisor;
      kappa -= 1;
      // Note that kappa now equals the exponent of the divisor and that the
      // invariant thus holds again.
      val rest = (ULong(integrals.toLong) << -one.e) + fractionals;
      // Invariant: too_high = buffer * 10^kappa + DiyFp(rest, one.e())
      // Reminder: unsafe_interval.e() == one.e()
      if(rest < unsafeIntervalF) {
        // Rounding down (by not emitting the remaining digits) yields a number
        // that lies within the unsafe interval.
        too_high -= w
        intBoxes.a = length
        intBoxes.b = kappa
        return roundWeed(buffer, length, too_high.f,
                         unsafeIntervalF, rest,
                         ULong(divisor.toLong) << -one.e, unit);
      }
      divisor /= UInt(10);
    }

    // The integrals have been generated. We are at the point of the decimal
    // separator. In the following loop we simply multiply the remaining digits by
    // 10 and divide by one. We just need to pay attention to multiply associated
    // data (like the interval or 'unit'), too.
    // Note that the multiplication by 10 does not overflow, because w.e >= -60
    // and thus one.e >= -60.
    assert(one.e >= -60);
    assert(fractionals < one.f);
    assert(ULong(-1L) / ULong(10) >= one.f);
    while (true) {
      fractionals *= ULong(10L);
      unit *= ULong(10L);
      unsafe_interval.f *= ULong(10);
      // Integer division by one.
      val digit = (fractionals >> -one.e).toInt;
      buffer(length) = ('0' + digit).toChar;
      length += 1;
      fractionals &= one.f - ULong(1);  // Modulo by one.
      kappa -= 1;
      if (fractionals < unsafe_interval.f) {
        too_high -= w
        intBoxes.a = length
        intBoxes.b = kappa
        return roundWeed(buffer, length, too_high.f * unit,
                         unsafe_interval.f, fractionals, one.f, unit);
      }
    }
    sys.error("Can't get here")
  }

  // Returns the biggest power of ten that is less than or equal to the given
  // number. We furthermore receive the maximum number of bits 'number' has.
  //
  // Returns power == 10^(exponent_plus_one-1) such that
  //    power <= number < power * 10.
  // If number_bits == 0 then 0^(0-1) is returned.
  // The number of bits must be <= 32.
  // Precondition: number < (1 << (number_bits + 1)).

  // Inspired by the method for finding an integer log base 10 from here:
  // http://graphics.stanford.edu/~seander/bithacks.html#IntegerLog10
  private final val kSmallPowersOfTenS = Array[Int](
      0, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000
    );
  private def kSmallPowersOfTen(i: Int) = UInt(kSmallPowersOfTenS(i))

  def biggestPowerTen(number: UInt,
                      number_bits: Int,
                      intBoxes: IntBoxes) // intBoxes is power.toInt, exponent_plus_one
  {
    assert(number < (UInt(1) << (number_bits + 1)));
    // 1233/4096 is approximately 1/lg(10).
    var exponent_plus_one_guess = ((number_bits + 1) * 1233 >> 12);
    // We increment to skip over the first entry in the kPowersOf10 table.
    // Note: kPowersOf10[i] == 10^(i-1).
    exponent_plus_one_guess += 1;
    // We don't have any guarantees that 2^number_bits <= number.
    // TODO(floitsch): can we change the 'while' into an 'if'? We definitely see
    // number < (2^number_bits - 1), but I haven't encountered
    // number < (2^number_bits - 2) yet.
    while (number < kSmallPowersOfTen(exponent_plus_one_guess)) {
      exponent_plus_one_guess-=1;
    }
    intBoxes.a = kSmallPowersOfTenS(exponent_plus_one_guess);
    intBoxes.b = exponent_plus_one_guess;
  }

  // Adjusts the last digit of the generated number, and screens out generated
  // solutions that may be inaccurate. A solution may be inaccurate if it is
  // outside the safe interval, or if we cannot prove that it is closer to the
  // input than a neighboring representation of the same length.
  //
  // Input: * buffer containing the digits of too_high / 10^kappa
  //        * the buffer's length
  //        * distance_too_high_w == (too_high - w).f() * unit
  //        * unsafe_interval == (too_high - too_low).f() * unit
  //        * rest = (too_high - buffer * 10^kappa).f() * unit
  //        * ten_kappa = 10^kappa * unit
  //        * unit = the common multiplier
  // Output: returns true if the buffer is guaranteed to contain the closest
  //    representable number to the input.
  //  Modifies the generated digits in the buffer to approach (round towards) w.
  def roundWeed(buffer: Array[Char],
                length: Int,
                distance_too_high_w: ULong,
                unsafe_interval: ULong,
                rest_ : ULong,
                ten_kappa: ULong,
                unit: ULong): Boolean =
  {
    var rest = rest_
    val small_distance = distance_too_high_w - unit;
    val big_distance = distance_too_high_w + unit;
    // Let w_low  = too_high - big_distance, and
    //     w_high = too_high - small_distance.
    // Note: w_low < w < w_high
    //
    // The real w (* unit) must lie somewhere inside the interval
    // ]w_low; w_high[ (often written as "(w_low; w_high)")

    // Basically the buffer currently contains a number in the unsafe interval
    // ]too_low; too_high[ with too_low < w < too_high
    //
    //  too_high - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    //                     ^v 1 unit            ^      ^                 ^      ^
    //  boundary_high ---------------------     .      .                 .      .
    //                     ^v 1 unit            .      .                 .      .
    //   - - - - - - - - - - - - - - - - - - -  +  - - + - - - - - -     .      .
    //                                          .      .         ^       .      .
    //                                          .  big_distance  .       .      .
    //                                          .      .         .       .    rest
    //                              small_distance     .         .       .      .
    //                                          v      .         .       .      .
    //  w_high - - - - - - - - - - - - - - - - - -     .         .       .      .
    //                     ^v 1 unit                   .         .       .      .
    //  w ----------------------------------------     .         .       .      .
    //                     ^v 1 unit                   v         .       .      .
    //  w_low  - - - - - - - - - - - - - - - - - - - - -         .       .      .
    //                                                           .       .      v
    //  buffer --------------------------------------------------+-------+--------
    //                                                           .       .
    //                                                  safe_interval    .
    //                                                           v       .
    //   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -     .
    //                     ^v 1 unit                                     .
    //  boundary_low -------------------------                     unsafe_interval
    //                     ^v 1 unit                                     v
    //  too_low  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    //
    //
    // Note that the value of buffer could lie anywhere inside the range too_low
    // to too_high.
    //
    // boundary_low, boundary_high and w are approximations of the real boundaries
    // and v (the input number). They are guaranteed to be precise up to one unit.
    // In fact the error is guaranteed to be strictly less than one unit.
    //
    // Anything that lies outside the unsafe interval is guaranteed not to round
    // to v when read again.
    // Anything that lies inside the safe interval is guaranteed to round to v
    // when read again.
    // If the number inside the buffer lies inside the unsafe interval but not
    // inside the safe interval then we simply do not know and bail out (returning
    // false).
    //
    // Similarly we have to take into account the imprecision of 'w' when finding
    // the closest representation of 'w'. If we have two potential
    // representations, and one is closer to both w_low and w_high, then we know
    // it is closer to the actual value v.
    //
    // By generating the digits of too_high we got the largest (closest to
    // too_high) buffer that is still in the unsafe interval. In the case where
    // w_high < buffer < too_high we try to decrement the buffer.
    // This way the buffer approaches (rounds towards) w.
    // There are 3 conditions that stop the decrementation process:
    //   1) the buffer is already below w_high
    //   2) decrementing the buffer would make it leave the unsafe interval
    //   3) decrementing the buffer would yield a number below w_high and farther
    //      away than the current number. In other words:
    //              (buffer{-1} < w_high) && w_high - buffer{-1} > buffer - w_high
    // Instead of using the buffer directly we use its distance to too_high.
    // Conceptually rest ~= too_high - buffer
    // We need to do the following tests in this order to avoid over- and
    // underflows.
    assert(rest <= unsafe_interval);
    while (rest < small_distance &&  // Negated condition 1
             unsafe_interval - rest >= ten_kappa &&  // Negated condition 2
             (rest + ten_kappa < small_distance ||  // buffer{-1} > w_high
                small_distance - rest >= rest + ten_kappa - small_distance))
    {
      buffer(length - 1) = (buffer(length - 1) - 1).toChar;
      rest += ten_kappa;
    }

    // We have approached w+ as much as possible. We now test if approaching w-
    // would require changing the buffer. If yes, then we have two possible
    // representations close to w, but we cannot decide which one is closer.
    if (rest < big_distance &&
          unsafe_interval - rest >= ten_kappa &&
          (rest + ten_kappa < big_distance ||
             big_distance - rest > rest + ten_kappa - big_distance))
    {
      return false;
    }

    // Weeding test.
    //   The safe interval is [too_low + 2 ulp; too_high - 2 ulp]
    //   Since too_low = too_high - unsafe_interval this is equivalent to
    //      [too_high - unsafe_interval + 4 ulp; too_high - 2 ulp]
    //   Conceptually we have: rest ~= too_high - buffer
    return (ULong(2) * unit <= rest) && (rest <= unsafe_interval - ULong(4) * unit);
  }

  private def createDecimalRepresentation(decimal_digits: Array[Char],
                                          length: Int,
                                          decimal_point: Int,
                                          digits_after_point: Int,
                                          writer: Writer): Unit =
  {
    // Create a representation that is padded with zeros if needed.
    if (decimal_point <= 0) {
      // "0.00000decimal_rep".
      writer.write('0');
      if (digits_after_point > 0) {
        writer.write('.');
        writer.write("0" * (-decimal_point));
        assert(length <= digits_after_point - (-decimal_point));
        writer.write(decimal_digits, 0, length);
        val remaining_digits = digits_after_point - (-decimal_point) - length;
        writer.write("0" * remaining_digits)
      }
    } else if (decimal_point >= length) {
      // "decimal_rep0000.00000" or "decimal_rep.0000"
      writer.write(decimal_digits, 0, length);
      writer.write("0" * (decimal_point - length));
      if (digits_after_point > 0) {
        writer.write('.');
        writer.write("0" * digits_after_point)
      }
    } else {
      // "decima.l_rep000"
      assert(digits_after_point > 0);
      writer.write(decimal_digits, 0, decimal_point);
      writer.write('.');
      assert(length - decimal_point <= digits_after_point);
      writer.write(decimal_digits, decimal_point,
                    length - decimal_point);
      val remaining_digits = digits_after_point - (length - decimal_point);
      writer.write("0" * remaining_digits);
    }
  }

  private def createExponentialRepresentation(decimal_digits: Array[Char],
                                              length: Int,
                                              exponent_ : Int,
                                              writer: Writer): Unit =
  {
    var exponent = exponent_
    assert(length != 0);
    writer.write(decimal_digits(0));
    if (length != 1)
    {
      writer.write('.');
      writer.write(decimal_digits, 1, length - 1);
    }
    writer.write(exponent_character);
    if (exponent < 0) {
      writer.write('-');
      exponent = -exponent;
    } else if (exponent == 0) {
      writer.write('0');
      return;
    }
    assert(exponent < 10000);
    if (exponent >= 100) {
      writer.write(('0' + exponent / 100).toChar);
      exponent %= 100;
      writer.write(('0' + exponent / 10).toChar);
      exponent %= 10;
      writer.write(('0' + exponent).toChar);
    } else if (exponent >= 10) {
      writer.write(('0' + exponent / 10).toChar);
      exponent %= 10;
      writer.write(('0' + exponent).toChar);
    } else {
      writer.write(('0' + exponent).toChar);
    }
  }
}
