effect.se <- function(effect=NA, p.value=NA) {
    z <- -0.862 + sqrt(0.743 - 2.404 * log(p.value))
    SE <- abs(effect/z)
    CI <- SE * 1.96
    U95 <- effect + CI
    L95 <- effect - CI
    list("SE"=SE, "effect"=effect, "p.value"=p.value)
}

oddratio.confint <- function(oddsratio=NA, p.value=NA) {
    z <- -0.862 + sqrt(0.743 - 2.404 * log(p.value))
    SE <- abs(log(oddsratio)/z)
    CI <- abs(SE * 1.96)
    U95 <- exp(log(oddsratio) + CI)
    L95 <- exp(log(oddsratio) - CI)
    list("SE"=SE, "conf.int"=CI, "oddsratio"=oddsratio, "p.value"=p.value, "L95"=L95, "U95"=U95)
}
