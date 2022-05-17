
mosty = read.csv('data_ver220516C.csv', sep=',', header=TRUE)

png('boxplot.png')
boxplot(mosty$pilir1, mosty$pilir2, mosty$pilir3, mosty$pilir4,
        main='napeti', names=c('p1', 'p2', 'p3', 'p4'))
dev.off()

attach(mosty)

p2.mean = mean(pilir2)
p2.sd = sd(pilir2)

quantile = qnorm(0.975, mean=p2.mean, sd=p2.sd)

lowerBound = 44 - quantile * p2.sd / sqrt(p2.mean)
upperBound = 44 + quantile * p2.sd / sqrt(p2.mean)

write(p2.mean, stdout())
write('\n')
write(lowerBound, stdout())
write('\n')
write(upperBound, stdout())
write('\n')

q2 = qnorm(0.99, mean=p2.mean, sd=p2.sd)
ub2 = 44 + q2 * p2.sd / sqrt(p2.mean)

write('pravostranny odhad:', stdout())
write(ub2, stdout())

shapiro.test(pilir1)$p.value
shapiro.test(pilir2)$p.value
shapiro.test(pilir3)$p.value
shapiro.test(pilir4)$p.value

bartlett.test(mosty)

m2 = stack(mosty)
colnames(m2) = c('hodnota', 'pilir')

res = aov(m2$hodnota~m2$pilir, data=m2)
summary(res)

TT = TukeyHSD(res)
TT

