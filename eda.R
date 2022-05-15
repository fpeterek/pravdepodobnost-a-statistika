
loziska = read.csv('loziska.csv', sep=';', header=FALSE)

machine.names = c('machine1', 'machine2', 'machine3', 'machine4')

names(loziska) = machine.names

attach(loziska)

# Pt. 1: Exploratory Data Analysis

write('a) Boxplot \n', stdout())

png(file='boxplot_op')
boxplot(machine1, machine2, machine3, machine4, main='Prumery lozisek', ylab='mm', names=machine.names)
dev.off()

png(file='boxplot_prumery.png')
boxplot(machine1, machine2, machine3, machine4, main='Prumery lozisek', ylab='mm', ylim=c(11.95, 12.05), names=machine.names)
dev.off()

# Drop outliers

# Thresholds are determined empirically from boxplots and are higher than they need to be
loziska[loziska <= 11.95] = NA
loziska[loziska >= 12.05] = NA

attach(loziska)

shapiro.test(machine1)$p.value
shapiro.test(machine2)$p.value
shapiro.test(machine3)$p.value
shapiro.test(machine4)$p.value

# H0: distribution is normal for machine number x
# HA: distribution is not normal for machine number x
write('(S-W Test) Na hladine vyznamnosti 0.05 nezamitame normalitu pro ani jeden stroj', stdout())

# Pt. 2: Machine 2 output verification

m1.var = var(machine1)
m1.sd = sd(machine1)
m1.count = nrow(loziska)
m1.mean = mean(machine1)

# H0: mean == 12.0
# HA: mean != 12.0
# alpha = 0.05

# H0: M1 ~ N(12.0, m1.var / m1.count)

# All four machines are assumed to run independetly of one another, thus
# we assume the four datasets are independent

quantile = qnorm(0.975, mean=m1.mean, sd=m1.sd)

# Inter-quantile range approx.
lowerBound = 12 - quantile * m1.sd / sqrt(m1.count)
upperBound = 12 + quantile * m1.sd / sqrt(m1.count)

write('b) Stredni hodnota \n', stdout())

if (lowerBound <= m1.mean && m1.mean <= upperBound) {
    write('Na hladine vyznamnosti 0.05 je stredni hodnota prumeru rovna 12.0', stdout())
} else {
    write('Na hladine vyznamnosti 0.05 stredni hodnota prumeru neni rovna 12.0', stdout())
}

t.test(x=machine1, y=machine2, mu=0, alternative='two.sided', var.equal=TRUE, conf.level=0.95)$p.value

write('c) Intervalovy odhad stredni hodnoty \n', stdout())

intervalStr = sprintf('Intervalovy odhad stredni hodnoty prumeru na hladine vyznamnosti 0.05 pro prvni stroj je (%f; %f)', lowerBound, upperBound)
write(intervalStr, stdout())

# Homoscedasticity - test using exploratory analysis
# H0: var_min / var_max <= 2
# HA: var_min / var_max > 2

m2.var = var(machine2)
m3.var = var(machine3)
m4.var = var(machine4[!is.na(machine4)])

var.max = max(c(m1.var, m2.var, m3.var, m4.var))
var.min = min(c(m1.var, m2.var, m3.var, m4.var))

write(var.max, stdout())
write(var.min, stdout())
write(c(m1.var, m2.var, m3.var, m4.var), stdout())

homoscedasticityStr = sprintf('Na hladine vyznamnosti 0.05 %szamitame hypotezu o homoskedasticite datasetu', if (var.min / var.max > 2) '' else 'ne')

write(homoscedasticityStr, stdout())

m2.mean = mean(machine2)
m3.mean = mean(machine3)
m4.mean = mean(machine4[!is.na(machine4)])

write('d) Rovnosti prumeru napric jednotlivymi stroji \n', stdout())

t.test(x=machine1, y=machine2, alternative='two.sided', mu=0, paired=TRUE, var.equal=TRUE, conf.level=0.95)$p.value
t.test(x=machine1, y=machine3, alternative='two.sided', mu=0, paired=TRUE, var.equal=TRUE, conf.level=0.95)$p.value
t.test(x=machine1, y=machine4, alternative='two.sided', mu=0, paired=TRUE, var.equal=TRUE, conf.level=0.95)$p.value

t.test(x=machine2, y=machine3, alternative='two.sided', mu=0, paired=TRUE, var.equal=TRUE, conf.level=0.95)$p.value
t.test(x=machine2, y=machine4, alternative='two.sided', mu=0, paired=TRUE, var.equal=TRUE, conf.level=0.95)$p.value

t.test(x=machine3, y=machine4, alternative='two.sided', mu=0, paired=TRUE, var.equal=TRUE, conf.level=0.95)$p.value

write('Na hladine vyznamnosti 0.05 nezamitame hypotezu o ekvivalenci prumeru pro stroje 1 a 4', stdout())
write('Na hladine vyznamnosti 0.05 zamitame hypotezu o ekvivalenci prumeru pro vsechny ostatni pary stroju', stdout())

