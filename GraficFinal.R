#In cadrul acestui fisier se va realiza graficul tuturor curbelor ROC obtinute prin metodele prezentate
#adf_nb - Naive Bayes
#adf_rl - Regresie Logistica
#adf_gini - Arbore cu parametru gini
colnames(adf_gini) <- c("specificity", "sensitivity")


ggplot() +
  geom_line(data = adf_nb, aes(x = specificity, y = sensitivity, color = "Naive Bayes"), show.legend = TRUE) +
  geom_line(data = adf_rl, aes(x = specificity, y = sensitivity, color = "Regresie logistica"), show.legend = TRUE) +
  geom_line(data = adf_gini, aes(x = specificity, y = sensitivity, color = "Arbore"), show.legend = TRUE) +
  scale_x_reverse() +
  labs(x = "Specificity", y = "Sensitivity", color = "Method") +
  scale_color_manual(values = c("Naive Bayes" = "green", "Regresie logistica" = "red", "Arbore" = "blue"))
