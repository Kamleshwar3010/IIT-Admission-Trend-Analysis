library(dplyr)
library(ggplot2)
library(plotrix)
setwd("C:/Users/Admin/Downloads/archive/")
csv<- read.csv("student_data.csv",header = TRUE, sep = ",")
csv_tbl<- as_tibble(csv)
head(csv_tbl)
#to see all column heading
colnames(csv)
#adding new column in dataset
discount_percentage<- (csv$Discount.on.Fees/csv$Fees)*100
discount_percentage<- round(discount_percentage,2)
csv$discount_percentage<-(discount_percentage)
#again checking all column heading
colnames(csv)
# lets check how many admission taken per year
admission_per_year<-(count(group_by(csv,Year.of.Admission)))
print(admission_per_year)
 
ggplot(admission_per_year, aes(x = Year.of.Admission, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "IIT Admission Trend (2016-2022)",
       x = "Year of Admission", y = "Number of Admissions") +
  geom_text(aes(label = n), hjust = 0.5, vjust = 20, color = "white") +
  theme_minimal()+geom_line()


# lets check how many student are in each deparment
field_with_most_students<-(count(group_by(csv,Field.of.Study)))
print(field_with_most_students)

ggplot(field_with_most_students, aes(x = Field.of.Study, y = n)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "IIT Admission Trend (2016-2022)",
       x = "Branch", y = "Number of Admissions") +
  geom_text(aes(label = n), hjust = 0.5, vjust = 20, color = "white") +
  theme_minimal()

department_percentage= (field_with_most_students$n/sum(field_with_most_students$n))*100
department_percentage=round(department_percentage,2)
department_percentage<-data.frame(Department = field_with_most_students$Field.of.Study, Percentage = department_percentage)
department_percentage
pie3D(department_percentage$Percentage,
      labels = department_percentage$Percentage,
      col = rainbow(length(department_percentage$Percentage)))
legend("bottomright", department_percentage$Department,
       cex = 0.5, fill = rainbow(length(department_percentage$Percentage)))
colnames(csv)

# Create scatter plot
pairs(~Fees + Discount.on.Fees, data = csv,
      col.axis = "darkgreen")
# Calculate the maximum and minimum values
max_fee <- max(csv$Fees)
min_fee <- min(csv$Fees)
max_discount <- max(csv$discount_percentage)
min_discount <- min(csv$discount_percentage)

# Create a data frame for the plot
data <- data.frame(
  category = c("Fees", "Discount"),
  value = c(max(csv$Fees), max(csv$Discount.on.Fees)),
  ymin = c(min(csv$Fees), min(csv$Discount.on.Fees)),
  ymax = c(max(csv$Fees), max(csv$Discount.on.Fees))
)

# Create the plot
ggplot(data, aes(x = category, y = value)) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 1.5, color = "red") +
  labs(title = "Maximum and Minimum Fees and Discounts",
       x = "Category", y = "Value") +
  theme_minimal()
