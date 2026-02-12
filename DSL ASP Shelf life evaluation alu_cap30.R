library(ggplot2)
time <- c(0, 3, 6, 9, 0, 3, 6, 9, 0, 3, 6, 9)
Any_imp <- c(0.085, 0.082, 0.104, 0.084, 
              0.091, 0.093, 0.086, 0.086,
              0.091, 0.084, 0.082, 0.082)
lot <- rep(c("lot_pv1", "lot_pv2", "lot_pv3"), each=4)
data <- data.frame(time, Any_imp, lot)
model <- lm(Any_imp ~ time, data=data)
summary(model)
shelf_life <- (0.2 - coef(model)[1]) / coef(model)[2]
print(paste("Estimated shelf-life:", round(shelf_life, 2), "months"))


new_time <- seq(0, 36, by=3)
predicted_Any_imp <- predict(model, newdata=data.frame(time=new_time))
predicted_data <- data.frame(time=new_time, Any_imp=predicted_Any_imp, lot="Prediction")
combined_data <- rbind(data, predicted_data)
ggplot(combined_data, aes(x=time, y=Any_imp, color=lot)) +
  geom_point(data=data, size=3) +  # จุดข้อมูลจริง
  geom_line(data=predicted_data, linetype="dashed", size=1) +  # เส้นพยากรณ์
  labs(title="Stability Study of Any individual impurity for Alu cap (Predicted to 36 Months)",
       x="Time (Months)", y="Impurity Level") +
  theme_minimal()



new_time <- seq(0, 36, by=1)
predictions <- predict(model, newdata=data.frame(time=new_time), interval="confidence", level=0.95)
predicted_data <- data.frame(time=new_time, Any_imp=predictions[,1],
                             lower=predictions[,2], upper=predictions[,3])
predicted_data <- na.omit(predicted_data)
if (any(predicted_data$upper >= 0.2)) {
  shelf_life_95CI <- approx(predicted_data$upper, predicted_data$time, xout=0.2)$y
} else {
  shelf_life_95CI <- NA  # ถ้าไม่มีค่าที่ถึง 0.1 ให้คืนค่า NA
}
print(paste("Shelf-life at 95% Confidence Level:", round(shelf_life_95CI, 2), "months"))

custom_breaks <- c(0, 3, 6, 9, 12,15,18,21,24,27,30,33,36)

ggplot() +
  geom_point(data=data, aes(x=time, y=Any_imp, color=lot), size=3) +  # จุดข้อมูลจริง
  geom_line(data=predicted_data, aes(x=time, y=Any_imp), color="blue", size=1) +  # เส้นพยากรณ์
  geom_ribbon(data=predicted_data, aes(x=time, ymin=lower, ymax=upper), fill="blue", alpha=0.2) +  # แสดง CI
  geom_hline(yintercept=0.2, linetype="dashed", color="red", size=1) +  # เส้น impurity limit
  {if (!is.na(shelf_life_95CI))geom_vline(xintercept=shelf_life_95CI, linetype="dotted", color="black", size=1)} +  # เส้น shelf-lif
  scale_x_continuous(breaks=custom_breaks) +
  labs(title="Shelf-life Prediction with 95% Confidence Interval",
       x="Time (Months)", y="Impurity Level") +
  theme_minimal()



