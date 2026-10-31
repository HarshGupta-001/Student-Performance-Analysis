
# ==========================================
# MINI PROJECT: STUDENT PERFORMANCE ANALYSIS
# Language: R
# Software: RStudio
# ==========================================

# 1. CREATE STUDENT DATASET

students <- data.frame(
  Student_ID = 1:15,
  Name = c("Amit", "Priya", "Rahul", "Neha", "Karan",
           "Sneha", "Rohit", "Pooja", "Vikas", "Anjali",
           "Arjun", "Simran", "Varun", "Isha", "Manav"),
  Math = c(85, 92, 67, 78, 56, 88, 72, 95, 60, 81,
           76, 89, 45, 93, 69),
  Science = c(80, 89, 70, 75, 58, 91, 68, 94, 62, 85,
              79, 86, 50, 96, 72),
  English = c(78, 95, 65, 80, 60, 86, 75, 90, 64, 88,
              82, 91, 48, 89, 70),
  Attendance = c(90, 95, 75, 85, 60, 92, 78, 98, 65, 88,
                 80, 94, 55, 97, 73)
)

# 2. CALCULATE TOTAL AND AVERAGE

students$Total <- students$Math +
  students$Science + students$English

students$Average <- round(students$Total / 3, 2)

# 3. ASSIGN GRADES

students$Grade <- ifelse(students$Average >= 90, "A+",
                         ifelse(students$Average >= 80, "A",
                                ifelse(students$Average >= 70, "B",
                                       ifelse(students$Average >= 60, "C", "D"))))

# 4. PASS OR FAIL

students$Result <- ifelse(
  students$Math >= 35 &
    students$Science >= 35 &
    students$English >= 35,
  "Pass", "Fail"
)

# 5. DISPLAY STUDENT DATA

cat("\n===== STUDENT PERFORMANCE REPORT =====\n")
print(students)

# 6. CLASS STATISTICS

cat("\n===== CLASS STATISTICS =====\n")

cat("Total Students:", nrow(students), "\n")

cat("Class Average:",
    round(mean(students$Average), 2), "\n")

cat("Highest Marks:",
    max(students$Average), "\n")

cat("Lowest Marks:",
    min(students$Average), "\n")

cat("Pass Percentage:",
    round(mean(students$Result == "Pass") * 100, 2), "%\n")

cat("Average Attendance:",
    round(mean(students$Attendance), 2), "%\n")

# 7. TOPPER AND LOWEST PERFORMER

top_student <- students[which.max(students$Average), ]

lowest_student <- students[which.min(students$Average), ]

cat("\n===== TOP PERFORMING STUDENT =====\n")
print(top_student[, c("Name", "Average", "Grade")])

cat("\n===== LOWEST PERFORMING STUDENT =====\n")
print(lowest_student[, c("Name", "Average", "Grade")])

# 8. SUBJECT-WISE AVERAGE

subject_avg <- colMeans(
  students[, c("Math", "Science", "English")]
)

cat("\n===== SUBJECT-WISE AVERAGE =====\n")
print(round(subject_avg, 2))

# 9. GRADE DISTRIBUTION

grade_count <- table(students$Grade)

cat("\n===== GRADE DISTRIBUTION =====\n")
print(grade_count)

# 10. GRAPH 1: STUDENT AVERAGE MARKS

barplot(
  students$Average,
  names.arg = students$Name,
  col = "skyblue",
  main = "Student Average Marks",
  xlab = "Students",
  ylab = "Average Marks",
  ylim = c(0, 100),
  las = 2
)

# 11. GRAPH 2: SUBJECT-WISE AVERAGE

barplot(
  subject_avg,
  col = c("orange", "lightgreen", "pink"),
  main = "Subject-wise Average Marks",
  xlab = "Subjects",
  ylab = "Average Marks",
  ylim = c(0, 100)
)

# 12. GRAPH 3: ATTENDANCE VS MARKS

plot(
  students$Attendance,
  students$Average,
  pch = 19,
  col = "blue",
  main = "Attendance vs Student Performance",
  xlab = "Attendance (%)",
  ylab = "Average Marks",
  xlim = c(0, 100),
  ylim = c(0, 100)
)

text(
  students$Attendance,
  students$Average,
  labels = students$Name,
  pos = 3,
  cex = 0.6
)

# 13. GRAPH 4: GRADE DISTRIBUTION

barplot(
  grade_count,
  col = c("gold", "lightgreen", "skyblue",
          "pink", "orange"),
  main = "Grade Distribution",
  xlab = "Grades",
  ylab = "Number of Students"
)

# 14. GRAPH 5: PASS AND FAIL PIE CHART

result_count <- table(students$Result)

pie(
  result_count,
  col = c("lightgreen", "tomato"),
  main = "Pass and Fail Distribution",
  labels = paste(names(result_count),
                 result_count, sep = ": ")
)

# 15. GRAPH 6: SUBJECT COMPARISON

barplot(
  t(as.matrix(students[, c("Math", "Science", "English")])),
  beside = TRUE,
  names.arg = students$Name,
  col = c("orange", "lightgreen", "skyblue"),
  main = "Student Subject Comparison",
  xlab = "Students",
  ylab = "Marks",
  ylim = c(0, 100),
  las = 2,
  legend.text = c("Math", "Science", "English"),
  args.legend = list(x = "topright", cex = 0.7)
)

# 16. GRAPH 7: ATTENDANCE HISTOGRAM

hist(
  students$Attendance,
  col = "lightblue",
  border = "black",
  main = "Attendance Distribution",
  xlab = "Attendance Percentage",
  ylab = "Number of Students",
  breaks = 5
)

# 17. GRAPH 8: TOTAL MARKS HISTOGRAM

hist(
  students$Total,
  col = "lightpink",
  border = "black",
  main = "Total Marks Distribution",
  xlab = "Total Marks",
  ylab = "Number of Students",
  breaks = 5
)

# 18. GRAPH 9: SUBJECT-WISE BOXPLOT

boxplot(
  students[, c("Math", "Science", "English")],
  col = c("orange", "lightgreen", "skyblue"),
  main = "Subject-wise Marks Distribution",
  xlab = "Subjects",
  ylab = "Marks"
)

# 19. GRAPH 10: CORRELATION AND REGRESSION

correlation <- cor(
  students$Attendance,
  students$Average
)

plot(
  students$Attendance,
  students$Average,
  pch = 19,
  col = "darkgreen",
  main = paste("Attendance vs Marks - Correlation:",
               round(correlation, 2)),
  xlab = "Attendance (%)",
  ylab = "Average Marks"
)

abline(
  lm(Average ~ Attendance, data = students),
  col = "red",
  lwd = 2
)

# 20. FINAL SUMMARY

cat("\n===== FINAL PROJECT SUMMARY =====\n")

cat("Total Students:", nrow(students), "\n")
cat("Class Average:", round(mean(students$Average), 2), "\n")
cat("Pass Percentage:",
    round(mean(students$Result == "Pass") * 100, 2), "%\n")
cat("Average Attendance:",
    round(mean(students$Attendance), 2), "%\n")
cat("Attendance-Marks Correlation:",
    round(correlation, 2), "\n")

cat("\n===== PROJECT COMPLETED SUCCESSFULLY =====\n")

# 21. EXPORT RESULT TO CSV

write.csv(
  students,
  "Student_Performance_Report.csv",
  row.names = FALSE
)

cat("\nStudent report exported successfully!\n")

