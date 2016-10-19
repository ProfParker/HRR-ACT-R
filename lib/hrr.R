
newHRR <- function(len) {
    a <- rnorm(len)
    class(a) <- "HRR"
    norm(a)
}

print.HRR <- function(v) {
    print(unclass(v))
}

norm <- function(v) {
    UseMethod("norm")
}
norm.HRR <- function(v) {
    v / mag(v)
}

mag <- function(v) {
    UseMethod("mag")
}
mag.HRR <- function(v) {
    sqrt(sum(unclass(v)^2))
}

dot <- function(a, b) {
    UseMethod("dot")
}
dot.HRR <- function(a, b) {
    sum(unclass(a) * unclass(b))
}

cosine <- function(a,b) {
    UseMethod("cosine")
}
cosine.HRR <- function(a, b) {
    dot(a, b) / (mag(a) * mag(b))
}

Ops.HRR <- function(e1, e2=NULL) {
    if (!is.element(.Generic, 
                    c("+", "-", "*", "!", "^", "/"))) {
        stop("operator ", .Generic, " not implemented for HRR")
    }

    l = class(e1) == "HRR"
    r = class(e2) == "HRR"

    e1 <- unclass(e1)
    e2 <- unclass(e2)

    if (.Generic=="+")      { a <- e1 + e2}
    else if (.Generic=="-") { a <- e1 - e2}
    else if (.Generic=="^") { a <- fft(fft(e1) ^ e2, inverse=TRUE) }
    else if (.Generic=="/") { a <- e1 / e2 }
    else if (.Generic=="*" && l && r) {
        a <- fft(fft(e1) * fft(e2), inverse=TRUE)
    }
    else if (.Generic=="*") { a <- e1 * e2}
    else if (.Generic=="!") { 
        a <- append(e1[seq(length(e1), 2, by=-1)], e1[1], after=0)
    }

    class(a) <- "HRR"
    Re(a)
}
