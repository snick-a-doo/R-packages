#' Calculate sample volume
#'
#' @param data A data frame with columns for p0, p1, p2, and fill direction.
#' @param cal A 1-line data frame with sample-filled and reference-filled chamber and
#' reference volumes, plus cup volume.
#' analysis.
#' @return Sample volume for each cycle.
#' @examples
#' ps <- read.set('balls-')
#' cal <- read.table('volumes')
#' stats(Vs(ps, cal))
#' @export
Vs <- function(data, cal) {
    p1g <- data$p1 - data$p0
    p2g <- data$p2 - data$p0
    factor <- p1g/p2g - 1
    sf <- data$fill == 'S'
    factor[sf] <- 1/factor[sf]
    Vc <- ifelse(sf, cal$Vcsf, cal$Vcrf)
    Vr <- ifelse(sf, cal$Vrsf, cal$Vrrf)
    Vc - cal$Vcup - Vr*factor
}

#plot.error <- function(rec, Vact, Vnom) {
#    rep.typ <- 1e-4*Vnom
#    rep.guar <- 1.5e-4*Vnom
#    acc <- 2e-4*(Vnom + Vact)
#    ggplot(rec) + geom_point(aes(n, volume), size=2) + geom_errorbar(aes(n,
#    ymin=volume-stddev, ymax=volume+stddev, width=0.5)) + labs(x='Analysis Number',
#    y='Volume (cm³)') + geom_hline(yintercept=Vact + c(0, rep.typ, -rep.typ, rep.guar,
#    -rep.guar, acc, -acc), color=c(8, 4, 4, 3, 3, 2, 2)) + scale_x_continuous(breaks=rec$n)
                                        #}
#' Plot volumes with error bars and limit lines
#'
#' @export
plot.error <- function(rec, Vact, Vnom) {
    rep.typ <- 1e-4*Vnom
    rep.guar <- 1.5e-4*Vnom
    acc <- 2e-4*(Vnom + Vact)
    ggplot(rec) + geom_point(aes(n, volume, color=fill), size=2) + geom_errorbar(aes(n,
    ymin=volume-stddev, ymax=volume+stddev, width=0.5)) + labs(x='Analysis Number',
    y='Volume (cm³)') + geom_hline(yintercept=Vact + c(0, rep.typ, -rep.typ, rep.guar,
    -rep.guar, acc, -acc), color=c(8, 4, 4, 3, 3, 2, 2)) + scale_x_continuous(breaks=rec$n)
}
