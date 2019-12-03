package com.rojoma.grisu

private[grisu] final class UInt(val toInt: Int) extends AnyVal {
  @inline
  def <<(bits: Int) = new UInt(toInt << bits)

  @inline
  def <(that: UInt): Boolean =
    java.lang.Integer.compareUnsigned(this.toInt, that.toInt) < 0

  @inline
  def *(that: UInt) = new UInt(this.toInt * that.toInt)
  @inline
  def -(that: UInt) = new UInt(this.toInt - that.toInt)
  @inline
  def /(that: UInt) = new UInt(java.lang.Integer.divideUnsigned(this.toInt, that.toInt))
  @inline
  def %(that: UInt) = this - (this/that)*that

  @inline
  def toLong = toInt.toLong & 0xffffffffL

  override def toString =
    java.lang.Integer.toUnsignedString(toInt)
}

private[grisu] object UInt {
  @inline
  def apply(x: Int) = new UInt(x)
}
