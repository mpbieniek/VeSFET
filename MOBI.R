library(ggplot2)

### STA£E ###

Uds1 <- 0.05 # [V]
Uds2 <- 0.8 # [V]

Ugs <- (c(0:(80/5))*5)/100

Id1 <- c(6E-16,1.75E-15,9.88E-15,5.54E-14,3.09E-13,1.70E-12,9.25E-12,4.94E-11,2.59E-10,1.31E-09,6.40E-09,2.91E-08,1.18E-07,3.89E-07,
                                                                                                            9.73E-07,1.85E-06,2.89E-06)

Id2 <- c(1.25E-15,7.06E-15,3.98E-14,2.24E-13,1.25E-12,6.90E-12,3.77E-11,2.03E-10,1.07E-09,5.50E-09,2.69E-08,1.20E-07,4.55E-07,1.35E-06,
                                                                                                            3.11E-06,5.78E-06,9.32E-06)
  
Uds <- c(0,0.025,0.05,0.075,(c(2:(80/5))*5)/100)

Ioff <- c(0.00,2.15E-16,3.09E-16,3.61E-16,4.00E-16,4.40E-16,4.84E-16,5.29E-16,5.78E-16,6.30E-16,6.86E-16,7.46E-16,
                                                                        8.12E-16,8.82E-16,9.50E-16,1.02E-15,1.10E-15,1.17E-15,1.25E-15)

# CHARAKTERYSTYKI PRZEJSCIOWE

#LINIOWE

dane <- data.frame(Ugs,Id1,Id2)

char_p_lin <- ggplot(dane, aes(Ugs)) + ggtitle("Charakterystyki przejœciowe") +
  geom_line(aes(y = Id1, colour = "0.05 V"), size = 1.5)+
  geom_line(aes(y = Id2, colour = "0.8 V"), size = 1.5)+
  xlab(expression(paste("Napiêcie Bramka - ród³o ", U[GS]," [V]")))+ labs(colour=expression(paste(U[DS]))) +
  ylab(expression(paste("Pr¹d drenu  ", I[D]," [A]")))+ theme_bw()+theme(axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),
                                                                axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16))
#LOGARYTMICZNE

char_p_log <- char_p_lin + scale_y_log10()

# IOFF OD UDS

#LINIOWE

dane1 <- data.frame(Uds,Ioff)

char_off_lin <- ggplot(dane1, aes(Uds)) + ggtitle(expression(paste("Zale¿noœæ pr¹du wy³¹czenia tranzystora od ", U[DS]))) +
  geom_line(aes(y = Ioff), size = 1.5)+
  xlab(expression(paste("Napiêcie Dren - ród³o ", U[GS]," [V]")))+
  ylab(expression(paste("Pr¹d wy³¹czenia  ", I[OFF]," [A]")))+ theme_bw()+theme(axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),
                                                                         axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16))
#LOGARYTMICZNE

char_off_log <- char_off_lin + scale_y_log10()
