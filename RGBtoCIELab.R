truthColourTableColumnNames <- c("r1", "g1", "b1", "r2", "g2", "b2")
truthColourTable <- read.csv("./colourcodes/colourcodes.csv", header=F)
names(truthColourTable) <- truthColourTableColumnNames


# Consider for XYZ reference values: https://www.mathworks.com/help/images/ref/whitepoint.html

sRGBtoXYZ <- function(sR, sG, sB) {
  # https://en.wikipedia.org/wiki/SRGB#The_reverse_transformation
  
  R = sR / 255
  G = sG / 255
  B = sB / 255
  
  if (R > 0.04045) {R = ((R + 0.055) / 1.055) ^ 2.4} else {R = R / 12.92}
  if (G > 0.04045) {G = ((G + 0.055) / 1.055) ^ 2.4} else {G = G / 12.92}
  if (B > 0.04045) {B = ((B + 0.055) / 1.055) ^ 2.4} else {B = B / 12.92}
  
  R = R * 100
  G = G * 100
  B = B * 100
  
  X = R * 0.4124 + G * 0.3576 + B * 0.1805
  Y = R * 0.2126 + G * 0.7152 + B * 0.0722
  Z = R * 0.0193 + G * 0.1192 + B * 0.9505
  
  return(c(X, Y, Z))
  
}

XYZtoCIELab <- function(X, Y, Z) {
  # https://en.wikipedia.org/wiki/CIELAB_color_space#From_CIEXYZ_to_CIELAB
  
  reference_X <- 95.0489 # TODO: Standard Illuminant D65 May need changing
  reference_Y <- 100 # TODO: Standard Illuminant D65 May need changing
  reference_Z <- 108.8840 # TODO:Standard Illuminant D65 May need changing
  
  X = X / reference_X
  Y = Y / reference_Y
  Z = Z / reference_Z
  
  delta = (6/29)^3
  
  if ( X > delta ) {X = X ^ ( 1/3 )} else {X = (X/(3*delta^2)) + (4/29)}                   
  if ( Y > delta ) {Y = Y ^ ( 1/3 )} else {Y = (Y/(3*delta^2)) + (4/29)}
  if ( Z > delta ) {Z = Z ^ ( 1/3 )} else {Z = (Z/(3*delta^2)) + (4/29)}
  
  CIE_L = ( 116 * Y ) - 16
  CIE_a = 500 * ( X - Y )
  CIE_b = 200 * ( Y - Z )
  
  return (c(CIE_L, CIE_a, CIE_b))
  
}

sRGBtoCIELab <-function(sR, sG, sB) {
  firstColourConversion <- sRGBtoXYZ(sR, sG, sB)
  return(XYZtoCIELab(firstColourConversion[1], firstColourConversion[2], firstColourConversion[3]))
}

firstColourSetCIELab <- t(apply(firstColourSet, 1, function (x) sRGBtoCIELab(x[[1]], x[[2]], x[[3]])))
secondColourSetCIELab <- t(apply(secondColourSet, 1, function (x) sRGBtoCIELab(x[[1]], x[[2]], x[[3]])))

colnames(firstColourSetCIELab) <- colnames(secondColourSetCIELab) <- c("L", "a", "b")
