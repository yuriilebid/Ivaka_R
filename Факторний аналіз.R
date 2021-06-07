# Імпортуємо масив SPSS, попередньо підключивши бібліотеку foreign. Слід вказати шлях до файлу і його назву
library(foreign)
data1 <- read.spss("C:/Markdown/ESS6UA.sav", to.data.frame = T)

# Щоб переглянути масив, створюємо змінну vars. Вона містить назви змінних, її мітки і клас змінної
vars <- data.frame(names(data1), 
                    attr(data1, "variable.labels"), 
                    sapply(data1, class))
# Кожній змінній присвоюємо порядковий номер (за допомогою rownames(vars))
rownames(vars) <- 1:624
colnames(vars) <- c("names", "labels", "class")

# Обираємо потрібні нам змінні для факторного аналізу (вказуємо порядкові номери змінних)
data_factor <- data1[,c(194:196, 208, 209)]
names(data_factor)
# Для зручності назвами змінних, відібраних для факторного аналізу, будуть виступати їхні мітки
names(data_factor) <- attr(data1, "variable.labels")[c(194:196, 208, 209)]
# Переглядаємо змінні, відібрані для факторного аналізу
sapply(data_factor, table)
# Ці змінні факторні; для того, щоб застосувати для них факторний аналіз, потрібно перетворити їх на числові (numeric)
data_factor <- as.data.frame(sapply(data_factor, as.numeric))
# Видаляємо пропущені значення
data_factor <- na.omit(data_factor)

# Встановлюємо пакет psych
install.packages("psych")
library("psych")

#Розраховуємо матрицю поліхоричних кореляцій
corr <- polychoric(data_factor)
# Розраховуємо довірчі інтервали для поліхоричних кореляцій. Якщо довірчий інтервал включає 0, значення кореляції не відрізняється від 0 
conf_int <- cor.ci(corr[["rho"]], poly = T, n = 1972)
conf_int
# Переглянемо матрицю кореляцій
correlations <- round(corr[["rho"]], 2)

# Визначимо кількість компонент/факторів, використавши паралельний аналіз
fa.parallel(data_factor, fm = "pa", fa = "both", cor = "poly")

# Метод головних компонент
# 1 компонента: 5 змінних
prin1 <- principal(data_factor, nfactors = 1, rotate = "varimax", cor = "poly")
prin1 <- fa.sort(prin1)
prin1
#1 компонента: 4 змінних (видалено змінну з найменшою загальністю)
prin2 <- principal(data_factor[,c(1:4)], nfactors = 1, rotate = "varimax", cor = "poly")
prin2 <- fa.sort(prin2)
prin2
# 2 компоненти
prin3 <- principal(data_factor, nfactors = 2, rotate = "varimax", cor = "poly")
prin3 <- fa.sort(prin3)
prin3
# Метод головних факторів
factor <- fa(data_factor, nfactors = 2, rotate = "varimax", fm = "pa", cor = "poly")
factor <- fa.sort(factor)
factor
