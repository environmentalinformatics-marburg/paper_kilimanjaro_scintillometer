################################################################################
# taken from http://cronklab.wikidot.com/calculation-of-vapour-pressure-deficit
################################################################################

## saturation vapor pressure
svp <- function(ta) {
  610.7 * 10^(7.5*ta/(237.3+ta))
}
# svp(25)

## actual vapor pressure
avp <- function(rh, svp) {
    rh * svp / 100
}
# avp(80, svp(25))

## vapor pressure difference
vpd <- function(ta, rh) {
  svp(ta) - avp(rh, svp(ta))
}
# vpd(25, 80)
