
# Load the readr and dplyr packages
# .... YOUR CODE FOR TASK 1 ....

# Read datasets kidney_stone_data.csv into data
data <- ....(....)

# Take a look at the first few rows of the dataset
# .... YOUR CODE FOR TASK 1 ....

library(testthat) 
library(IRkernel.testthat)

# One or more tests of the student's code
# The @solution should pass the tests
# The purpose of the tests is to try to catch common errors and
# to give the student a hint on how to resolve these errors

# There are two tests in this cell. The first one tests that the
# correct package was loaded. The second one tests that the
# correct data were read in.

ks_data <- read_csv("datasets/kidney_stone_data.csv")

run_tests({
    test_that("the correct package is loaded", {
        expect_true("readr" %in% .packages(),
                    "dplyr" %in% .packages(),
                    info = "Did you load readr?")
        })
    test_that("the dataset is loaded correctly", {
        expect_identical(colnames(data), colnames(ks_data), 
                         info = "data contains the wrong column names. Did you import the CSV file correctly?")    
        expect_equal(data, ks_data, 
                     info = "data contains the wrong values. Did you import the CSV file correctly?")
    })
})

# Calculate the number and frequency of success and failure of each treatment 
data %>% 
  group_by(...., ....) %>%
  summarise(N = ....) %>% 
  mutate(Freq = round(..../....(N), ....))

last_value <- .Last.value

correctout <- ks_data %>% 
  group_by(treatment, success) %>%
  summarise(N = n()) %>%
  mutate(Freq = round(N/sum(N),3))



run_tests({
    test_that("the answer is correct", {
        expect_identical(
            colnames(last_value), 
            colnames(correctout),
            info = "Output has the wrong columns. Did you create a `Freq` column?"
        )    
        expect_equal(
            as.data.frame(last_value),
            as.data.frame(correctout),
            info = "Output contains the wrong values.\n Did you calculate N using n(), and calculate Freq by dividing N by the sum of N? \n Did you round to the correct number of decimal points?"
        )
    })
        
})

# Calculate number and frequency of success and failure by stone size for each treatment
sum_data <- 
  .... %>% 
  group_by(treatment, ...., ....) %>%
  summarise(N = ....) %>%
  mutate(Freq = round(..../....(N),...))

# Print out the data frame we just created
# .... YOUR CODE FOR TASK 3 ....

last_value <- sum_data

correctout <- ks_data %>% 
  group_by(treatment, stone_size, success) %>%
  summarise(N = n()) %>%
  mutate(Freq = round(N/sum(N),3))



run_tests({
    test_that("the answer is correct", {
        expect_identical(
            colnames(last_value), 
            colnames(correctout),
            info = "Output has the wrong columns. Did you create a `N` and a `Freq` column?"
        )    
        expect_equal(
            as.data.frame(last_value),
            as.data.frame(correctout),
            info = "Output contains the wrong values.\n Did you calculate N using n(), and calculate Freq by dividing N by the sum of N? \n Did you round to the correct number of decimal points?"
        )
    })
    
})

# Load ggplot2
# .... YOUR CODE FOR TASK 4 ....

# Create a bar plot to show stone size count within each treatment
sum_data %>%
  ggplot(aes(x = ...., y = ....)) + 
  ....(aes(fill = ....), stat=....) 

library(stringr)
p <- last_plot()


p_correct <- correctout %>%
  ggplot(aes(x = treatment, y = N)) + 
  geom_bar(aes(fill = stone_size), stat = "identity") 

run_tests({
    test_that("correct columns are plotted", {
        mappings <- str_replace(as.character(p$mapping), "~", "")
        expect_equal(p$mapping, p_correct$mapping, 
            info = "You should plot treatment on the x-axis, N on the y-axis.")
    })
    
    test_that("Fill var is set correctly", {
    expect_equivalent(p$labels$fill,"stone_size", 
            info = "You should color fill the bar by stone_size")
    })
    
    test_that("Stat is set correctly", {
    expect_true('StatIdentity' %in% class(p$layers[[1]]$stat), 
            info = "stat should be set to 'identity'")
    })
})


# Load the broom package 
# .... YOUR CODE FOR TASK 5 ....

# Run a Chi-squared test
trt_ss <- ....(data$treatment, ....)

# Print out the result in tidy format 
....(trt_ss)

trt_ss_correct<- chisq.test(ks_data$treatment, ks_data$stone_size)
last_value  <-  .Last.value
tidy_correct <- tidy(trt_ss_correct)

run_tests({
    test_that("chi2 test is calculated correctly", {
    expect_equivalent(trt_ss$p, trt_ss_correct$p, 
        info = "Chi-squared test should be used to test the association between treatment and stone_size")
    })
    
    test_that("applied the correct tidy function",{
        expect_equivalent(last_value$statistic, tidy_correct$statistic,
                         info = 'You should use the `tidy()` function on the test output')
    })
})

# Run a multiple logistic regression
m <- ....(data = data, .... ~  .... + ...., family = ....)

# Print out model coefficient table in tidy format
# .... YOUR CODE FOR TASK 6 ....

lastvalue <-  .Last.value

model_correct <- tidy(glm(data = ks_data, success ~ stone_size + treatment, family = 'binomial'))

run_tests({
    test_that("the output is a data frame", {
    expect_that(lastvalue, is_a("data.frame"),
        info = "The final output should be a data frame.")
    })

    test_that("the model family is binomial", {
    expect_equivalent(m$family$family, "binomial", 
        info = "The model family should be binomial.")
    })
    
    
    test_that("the model coefficient table is correct", {
    expect_equal(sum(round(lastvalue$estimate,4)), sum(round(model_correct$estimate,4)), 
        info = "The model coefficient table should have three terms and printed using tidy().")
    })
})

# Save the tidy model output into an object
tidy_m <- ....(m)

# Plot the coefficient estimates with 95% CI for each term in the model
tidy_m %>%
  ggplot(aes(x = ...., y = ....)) + 
  geom_pointrange(aes(ymin = estimate - 1.96 * std.error, 
                      ymax = .... + 1.96 * ....)) +
  ....(yintercept = ....)

p <- last_plot()

p_correct <- model_correct %>%
  ggplot(aes(x=term, y=estimate)) + 
  geom_pointrange(aes(ymin=estimate-1.96*std.error, 
                      ymax=estimate+1.96*std.error)) +
  geom_hline(yintercept = 0)


run_tests({
    test_that("correct columns are plotted", {
        xmapping <- as.character(p$mapping$x)[2]
        expect_equal("term", xmapping, 
            info = "You should plot term on the x-axis.")
    })
    
    test_that("correct columns are plotted", {
        ymapping <- as.character(p$mapping$y)[2]
         expect_equal("estimate", ymapping, 
            info = "You should plot estimate on the y-axis.")
    })
    
    test_that("Upper CI is correct", {
    expect_equivalent(p$labels$ymax,"estimate + 1.96 * std.error", 
            info = "Upper CI is estimated as 'estimate + 1.96 * std.error'")
    })
    
    test_that("Stat is set correctly", {
    expect_true('GeomHline' %in% class(p$layers[[2]]$geom), 
            info = "stat should be set to 'identity'")
    })
    
    test_that("the y-intercept in geom_hline is correct", {
      expect_equal(p$layers[[2]]$data, p_correct$layers[[2]]$data,
            info = "the y-intercept in geom_hline() should be 0.")
    })
})


# Is small stone more likely to be a success after controlling for treatment option effect?
# Options: Yes, No (as string)
small_high_success <- ....

# Is treatment A significantly better than B?
# Options: Yes, No (as string)
A_B_sig <- ....


run_tests({
    test_that("q1 is correct", {
    expect_equal(small_high_success,"Yes", 
            info = "Expecting 'Yes'")
    })
    
    test_that("q2 is correct", {
    expect_equal(A_B_sig,'No',
            info = "Expecting 'No'")
    })
})
