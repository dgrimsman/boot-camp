read.pslg = function (file) {
  dat <- scan(file, quiet = TRUE)
  N.vert <- dat[1]
  N.dims <- dat[2]
  N.attr <- dat[3]
  N.boun <- dat[4]
  offset <- 4
  line.length <- 3 + N.attr + N.boun
  P <- matrix(NA, N.vert, 2)
  if (N.attr >= 1) {
    PA <- matrix(NA, N.vert, N.attr)
  } else {
    PA <- NA
  }
  if (N.boun >= 1) {
    PB <- matrix(NA, N.vert, N.boun)
  }
  else {
    PB <- NA
  }
  for (i in (1:N.vert)) {
    P[i, ] <- dat[offset + ((i - 1) * line.length) + (2:3)]
    if (N.attr >= 1) {
      PA[i, ] <- dat[offset + ((i - 1) * line.length) + 
                       3 + (1:N.attr)]
    }
    if (N.boun >= 1) {
      PB[i, ] <- dat[offset + ((i - 1) * line.length) + 
                       3 + N.attr + (1:N.boun)]
    }
  }
  offset <- offset + line.length * N.vert
  N.seg <- dat[offset + 1]
  N.boun <- dat[offset + 2]
  offset <- offset + 2
  line.length <- 3 + N.boun
  S <- matrix(NA, N.seg, 2)
  for (i in (1:N.seg)) {
    S[i, ] <- dat[offset + ((i - 1) * line.length) + (2:3)]
  }
  offset <- offset + line.length * N.seg
  N.hole <- dat[offset + 1]
  offset <- offset + 1
  H <- matrix(NA, N.hole, 2)
  for (i in (1:N.hole)) {
    H[i, ] <- dat[offset + ((i - 1) * 2) + (2:3)]
  }
  return(pslg(P = P, PA = PA, PB = PB, S = S, H = H))
}
