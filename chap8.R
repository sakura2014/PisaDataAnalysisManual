# データ準備
bel <- read.csv("pisa2006stu_BEL.csv", stringsAsFactors = FALSE)
bel$HISEI[bel$HISEI > 90] <- NA

# table 8.2
final_ms <- sapply(1:5, function(i) {
  weighted.mean(bel[[paste0("PV", i, "SCIE")]], bel$W_FSTUWT)
})
rep_ms <- lapply(1:80, function(j) {
  sapply(1:5, function(i) {
    weighted.mean(bel[[paste0("PV", i, "SCIE")]], bel[[paste0("W_FSTR", j)]])
  })
})
sq_ds <- do.call("rbind", lapply(rep_ms, function(x) x - final_ms))^2
ses <- sqrt(apply(sq_ds, 2, sum) / 20)
round(final_ms, 2)
round(do.call("rbind", rep_ms), 2)
round(ses, 2)

# p.120
final_m <- mean(final_ms)
final_v <- mean(ses^2)
imp_v <- var(final_ms)
final_err <- final_v + 1.2 * imp_v
final_se <- sqrt(final_err)
final_se

# box 8.2
intsvy::pisa.mean.pv(pvlabel = "SCIE", data = bel)

# box 8.3
intsvy::pisa.mean.pv(pvlabel = "SCIE", by = "ST04Q01", data = bel)

# box 8.4
bel$gender <- car::recode(bel$ST04Q01, "1=1; 2=0")
intsvy::pisa.reg(y = "PV1SCIE", x = c("HISEI", "gender"), data = bel)

# table 8.6
final_es <- sapply(1:5, function(i) {
  lm(bel[[paste0("PV", i, "SCIE")]] ~ bel$HISEI + bel$gender, weights = bel$W_FSTUWT)[[1]][2]
})
names(final_es) <- c("PV1", "PV2", "PV3", "PV4", "PV5")
rep_es <- lapply(1:80, function(j) {
  sapply(1:5, function(i) {
    lm(bel[[paste0("PV", i, "SCIE")]] ~ bel$HISEI + bel$gender, weights = bel[[paste0("W_FSTR", j)]])[[1]][2]
  })
})
sq_ds <- do.call("rbind", lapply(rep_es, function(x) x - final_es))^2
sv <- sqrt(apply(sq_ds, 2, sum) / 20)
names(sv) <- c("PV1", "PV2", "PV3", "PV4", "PV5")

round(final_es, 2)
round(do.call("rbind", rep_es), 2)
round(sv, 2)

# box 8.5
intsvy::pisa.reg.pv(x = c("HISEI", "gender"), pvlabel = "SCIE", data = bel)

# table 8.8
# intsvyでは計算できないので自力で計算
cor_wt <- function(x, y, w) {
  data <- na.omit(data.frame(x, y, w))
  cov.wt(x = data[1:2], wt = data[[3]], cor = TRUE)$cor[2]
}

final_ms <- sapply(1:5, function(i) {
  cor_wt(bel[[paste0("PV", i, "SCIE")]], bel$HISEI, bel$W_FSTUWT)
})
rep_ms <- lapply(1:80, function(j) {
  sapply(1:5, function(i) {
    cor_wt(bel[[paste0("PV", i, "SCIE")]], bel$HISEI, bel[[paste0("W_FSTR", j)]])
  })
})
sq_ds <- do.call("rbind", lapply(rep_ms, function(x) x - final_ms))^2
ses <- sqrt(apply(sq_ds, 2, sum) / 20)

mean(final_ms)
sqrt(mean(ses^2) + 1.2 * sd(ses^2))


# table 8.9
deu <- read.csv("pisa2003stu_DEU.csv")
intsvy::pisa.rho(variables = c(
  "PV1MATH1", "PV2MATH1", "PV3MATH1", "PV4MATH1", "PV5MATH1",
  "PV1MATH4", "PV2MATH4", "PV3MATH4", "PV4MATH4", "PV5MATH4"
), data = deu)[, 1:10][6:10, ]
