# Mutiara Nur Amalina
# 22106010089
# UTS Analisi Data

# A. Formulate your question 
# "Apakah ada hubungan antara total waktu yang dihabiskan oleh pengguna
#dan persentase konten kursus yang diselesaikan pengguna?"

# B. Read in your data
data <- read.csv("online_course_engagement_data.csv", header = TRUE)
cat("Dataset berhasil dibaca. Menampilkan 5 baris pertama:\n")
head(data)

# C. Check the packaging
str(data)
summary(data)
sapply(data, class)
colSums(is.na(data))
duplicated_rows <- sum(duplicated(data))
cat("Jumlah baris duplikat:", duplicated_rows, "\n")

# D. Look at the top and the bottom of your data
head(data)
tail(data)

# E. Check your "n"s
library(dplyr)

total_rows <- nrow(data)
cat("Jumlah total observasi:", total_rows, "\n")

unique_completion_rate <- length(unique(data$CompletionRate))
unique_time_spent <- length(unique(data$TimeSpentOnCourse))
cat("Jumlah nilai unik Completion Rate:", unique_completion_rate, "\n")
cat("Jumlah nilai unik TimeSpentOn Course:", unique_time_spent, "\n")

completion_rate_freq <- as.data.frame(table(data$CompletionRate))
cat("\nFrekuensi Completion Rate (5 nilai pertama):\n")
print(head(completion_rate_freq))

time_spent_freq <- as.data.frame(table(data$TimeSpentOnCourse))
cat("\nFrekuensi Time Spent On Course (5 nilai pertama):\n")
print(head(time_spent_freq))

# Filter kriteria waktu >10 jam dan CompletionRate >50%
filtered_data <- data %>%
  filter(TimeSpentOnCourse > 10 & CompletionRate > 50)  
cat("\nJumlah observasi dengan Time Spent On Course > 10 dan Completion Rate > 50%:", nrow(filtered_data), "\n")

cat("\nObservasi pertama setelah filter:\n")
print(head(filtered_data))


# F. Validate with at least one external data source
summary(data)

# G. Make a plot
# Histogram untuk TimecSpent On Course dan Completion Rate
library(ggplot2)
ggplot(data, aes(x = TimeSpentOnCourse)) +
  geom_histogram(binwidth = 5, fill = "purple", alpha = 0.7) +
  labs(title = "Distribusi Time Spent On Course", x = "Time Spent on Course (jam)", y = "Frekuensi") +
  theme_minimal()

ggplot(data, aes(x = CompletionRate)) +
  geom_histogram(binwidth = 5, fill = "orange", alpha = 0.7) +
  labs(title = "Distribusi Completion Rate", x = "Completion Rate (%)", y = "Frekuensi") +
  theme_minimal()
library(ggplot2)
library(dplyr)

data <- data %>%
  mutate(TimeSpentCategory = cut(TimeSpentOnCourse,
                                 breaks = c(0, 5, 10, 15, 20, Inf),
                                 labels = c("0-5 jam", "5-10 jam", "10-15 jam", "15-20 jam", ">20 jam"),
                                 right = FALSE))

ggplot(data, aes(x = TimeSpentCategory, y = CompletionRate)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Boxplot: Time Spent Categories vs Completion Rate",
       x = "Kategori Waktu (Jam)",
       y = "Completion Rate (%)") +
  theme_minimal()

data_summary <- data %>%
  group_by(TimeSpentCategory) %>%
  summarise(AverageCompletionRate = mean(CompletionRate, na.rm = TRUE))

ggplot(data_summary, aes(x = TimeSpentCategory, y = AverageCompletionRate)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "darkgreen") +
  labs(title = "Rata-rata Completion Rate berdasarkan Kategori Waktu",
       x = "Kategori Waktu (Jam)",
       y = "Rata-rata Completion Rate (%)") +
  theme_minimal()

# H. Try the easy solution first
# Uji korelasi Pearson
correlation <- cor(data$TimeSpentOnCourse, data$CompletionRate, use = "complete.obs")
cat("Korelasi antara TimeSpentOnCourse dan CompletionRate:", correlation, "\n")

# I. Follow up
# Membuat model regresi linear
model <- lm(CompletionRate ~ TimeSpentOnCourse, data = data)
summary(model)

# Models as Expectations 
  mean_completion_rate <- mean(data$CompletionRate, na.rm = TRUE)
sd_completion_rate <- sd(data$CompletionRate, na.rm = TRUE)

x_norm <- seq(0, 100, length.out = 1000)  
y_norm <- dnorm(x_norm, mean = mean_completion_rate, sd = sd_completion_rate)

library(ggplot2)
ggplot(data.frame(x = x_norm, y = y_norm), aes(x = x, y = y)) +
  geom_line(color = "maroon", size = 1) +
  labs(
    title = "Distribusi Normal Sebagai Ekspektasi",
    x = "Completion Rate",
    y = "Density"
  ) +
  theme_minimal()


# Comparing Model Expectations to Reality
ggplot(data, aes(x = CompletionRate)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "pink", alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean_completion_rate, sd = sd_completion_rate), 
                color = "maroon", size = 1) +
  labs(
    title = "Perbandingan Data Completion Rate dan Model Ekspektasi",
    x = "Completion Rate",
    y = "Density"
  ) +
  theme_minimal()
