# データ準備
deu <- read.csv("pisa2003stu_DEU.csv", stringsAsFactors = FALSE)
deu$HISEI[deu$HISEI == 99] <- NA
deu$ST03Q01[deu$ST03Q01 == 8] <- NA
deu$ST01Q01[deu$ST01Q01 > 90] <- NA
deu$BSMJ[deu$BSMJ > 90] <- NA


# p.104 Box 7.1
hisei_m <- weighted.mean(deu$HISEI, deu$W_FSTUWT, na.rm = TRUE)

weighted.mean(deu$HISEI, deu$W_FSTR1, na.rm = TRUE)
weighted.mean(deu$HISEI, deu$W_FSTR2, na.rm = TRUE)

weighted.mean(deu$HISEI, deu$W_FSTR79, na.rm = TRUE)
weighted.mean(deu$HISEI, deu$W_FSTR80, na.rm = TRUE)

hisei_rm <- sapply(1:80, function(i) {
  weighted.mean(deu$HISEI, deu[[paste0("W_FSTR", i)]], na.rm = TRUE)
})

# table 7.1
round(hisei_rm, 2)

# table  7.2
round((hisei_rm - hisei_m)^2, 4)
sum((hisei_rm - hisei_m)^2)

# p.105
# 2. sampling variance
sv <- sum((hisei_rm - hisei_m)^2) / 20
# 3. standard error
sqrt(sv)

# box 7.2
intsvy::pisa.mean(variable = "HISEI", data = deu)

# box 7.3
# 推定値がズレている・・・？
intsvy::pisa.mean(variable = "HISEI", by = "ST03Q01", data = deu)

# box 7.4
intsvy::pisa.table(variable = "ST03Q01", data = deu)

# table 7.7
final_w <- memisc::percent(deu$ST03Q01, deu$W_FSTUWT)[1]
rep_ws <- sapply(1:80, function(i) {
  memisc::percent(deu$ST03Q01, deu[[paste0("W_FSTR", i)]], na.rm = TRUE)[1]
})
sq_d <- (rep_ws - final_w)^2
round(final_w, 2)
round(rep_ws, 2)
round(sq_d, 2)
round(sum(sq_d), 2)

# box 7.5
intsvy::pisa.table(variable = "ST01Q01", by = c("ST03Q01"), data = deu)

# box 7.6
# AUTは省略
deu$gender <- car::recode(deu$ST03Q01, "1=1; 2=0")
intsvy::pisa.reg(y = "BSMJ", x = c("HISEI", "gender"), data = deu)

# box 7.7
intsvy::pisa.reg(y = "BSMJ", x = c("HISEI"), by = c("gender"), data = deu)

# box 7.8
intsvy::pisa.rho(variables = c("HISEI", "BSMJ"), data = deu)
