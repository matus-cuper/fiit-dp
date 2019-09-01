df.plot1 <- data.frame(value = IRELAND$X2812.csv[s:e], type = 1)
df.plot2 <- data.frame(value = IRELAND$X2812.csv[s:e], type = 2)
df.plot3 <- data.frame(value = IRELAND_SYNTHETIC$X2812.csv[s:e], type = 3)
df.plot1[c(98:145,434:481), ] <- NA
df.plot2[c(1:97,146:433,482:672), ] <- NA
df.plot3[c(1:96,147:432,483:672), ] <- NA

c(98:145,434:481)       # changed
c(1:97,146:433,482:672) # original

ggplot() + geom_line(aes(x = s:e, y = df.plot1$value), size = 1) +
  geom_line(aes(x = s:e, y = df.plot2$value), size = 1, linetype = "dashed", color = "grey") +
  geom_line(aes(x = s:e, y = df.plot3$value), size = 1, color = "blue") +
  xlab("Measurement ID") + ylab("Electricity consumption in kW") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.background = element_rect(fill = "white", color = "black")
  )
