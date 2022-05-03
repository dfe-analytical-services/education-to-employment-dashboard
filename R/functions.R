# For example, if rounded to nearest 10 then use round_significant(x, -1)
round_significant = function(x, n) {
  # get the sign of the number
  posneg = sign(x)
  # times the number (without sign - always pos by ten to the power of the number of digits wanted)
  z = abs(x)*10^n
  # add 0.5 to the value
  z = z + 0.5
  # discard everything after the decimal point
  z = trunc(signif(z)) #z = trunc(z)
  # divide number by ten to the power of the number of digits wanted
  z = z/10^n
  # re-add the pos/neg sign
  z*posneg
}
