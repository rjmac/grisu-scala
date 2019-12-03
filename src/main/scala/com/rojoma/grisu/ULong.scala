package com.rojoma.grisu

private[grisu] final class ULong(val toLong: Long) extends AnyVal {
  @inline
  def >=(that: ULong): Boolean =
    java.lang.Long.compareUnsigned(this.toLong, that.toLong) >= 0

  @inline
  def <=(that: ULong): Boolean =
    java.lang.Long.compareUnsigned(this.toLong, that.toLong) <= 0

  @inline
  def <(that: ULong): Boolean =
    java.lang.Long.compareUnsigned(this.toLong, that.toLong) < 0

  @inline
  def >(that: ULong): Boolean =
    java.lang.Long.compareUnsigned(this.toLong, that.toLong) < 0

  @inline
  def -(that: ULong) = new ULong(this.toLong - that.toLong)

  @inline
  def >>(bits: Int) = new ULong(toLong >>> bits)
  @inline
  def &(that: ULong) = new ULong(this.toLong & that.toLong)
  @inline
  def |(that: ULong) = new ULong(this.toLong | that.toLong)
  @inline
  def *(that: ULong) = new ULong(this.toLong * that.toLong)
  @inline
  def +(that: ULong) = new ULong(this.toLong + that.toLong)
  @inline
  def <<(bits: Int) = new ULong(toLong << bits)

  @inline
  def /(that: ULong) = new ULong(java.lang.Long.divideUnsigned(this.toLong, that.toLong))

  @inline
  def toInt = toLong.toInt

  override def toString =
    java.lang.Long.toUnsignedString(toLong)
}

private[grisu] object ULong extends (Long => ULong) {
  @inline
  def apply(x: Long) = new ULong(x)
}
