# Unit theory LTC calculation.
ut_ltc_single <- function(first, last, t1, learning, rate, lac = F) {
  qty <- last - first + 1
  lc <- sum((first:last)^learning)
  rc <- qty^rate
  ltc <- t1*lc*rc
  
  if (lac) {
    ltc/qty
  } else {
    ltc
  }
}

ut_ltc <- Vectorize(ut_ltc_single)



# Converts a sequence of cumulative averages to lot total costs
cac_to_ltc <- function(cac, last) {
  cumsum <- cac*last
  cumsum - dplyr::lag(cumsum, default = 0)
}



# CUMAV-Direct CAC and LTC calculation
cad_cac <- function(first, last, t1, learning, rate) {
  lc <- last^learning
  rc <- (last - first + 1)^rate
  t1*lc*rc
}

cad_ltc <- function(first, last, t1, learning, rate, lac = F) {
  ltc <- cac_to_ltc(cad_cac(first, last, t1, learning, rate),
                    last)
  
  if (lac) {
    ltc/(last - first + 1)
  } else {
    ltc
  }
}



# CUMAV-Iterative LTC calculation
cai_ltc <- function(first, last, t1, learning, rate, lac) {
  qty <- last - first + 1
  lc <- last^(learning + 1) - (first - 1)^(learning + 1)
  rc <- qty^rate
  ltc <- t1*lc*rc
  
  if (lac) {
    ltc/qty
  } else {
    ltc
  }
}



# Converts quantity stream to unit sequencing (first and last units)
stream_to_seq <- function(qty) {
  tibble(Qty = qty) %>%
    mutate(Last = cumsum(Qty),
           First = lag(Last, default = 0) + 1,
           Lot = row_number())
}






