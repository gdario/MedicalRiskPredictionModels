library(MedicalRiskPredictionModels)
prepareExamples()

# Add an event10y column that takes the value 1 if the patient suffers an
# event within 10 years and 0 otherwise
octrain$event10y <- as.integer(
  (octrain$survtime < 120) & (octrain$survstatus == 1)
)
octest$event10y <- as.integer(
  (octest$survtime < 120) & (octest$survstatus == 1)
)

# Logistic regression
fit1 <- glm(event10y ~ age + tumorthickness + grade, data = octrain,
            family = "binomial")
fit2 <- cph(Surv(survtime, survstatus) ~ age + tumorthickness + grade,
            data = octrain, x = TRUE, y = TRUE)

score_lr <- Score(
  list("lr" = fit1),
  formula = event10y ~ 1,
  data = octest,
  summary = "riskquantiles",
  plots = c("calibration")
)

score_cox <- Score(
  list("cox" = fit2),
  formula = Surv(survtime, survstatus) ~ 1,
  data = octest,
  times = 120,
  summary = "riskquantiles",
  plots = c("ROC", "calibration")
)

png("cox_calibration.png", 800, 800)
plotCalibration(score_cox)
dev.off()

png("lr_calibration.png", 800, 800)
plotCalibration(score_lr)
dev.off()
