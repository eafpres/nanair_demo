#
# test nanair
#
  library(ggplot2)
  library(naniar)
  library(simputation)
  library(UpSetR)
  library(dplyr)
  library(grid)
#
  rm(list = ls())
#
# read data and show the use of naniar's replace_with_na_all
# the formulate ~.x == "" means set all blank cells to NA
# nanair can replace multiple values at once
#
  train_data <- read.csv("train_original.csv", 
                         header = TRUE,
                         stringsAsFactors = FALSE) %>%
    replace_with_na_all(condition = ~.x == "")

  test_data <- read.csv("test_original.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE) %>%
  replace_with_na_all(condition = ~.x == "")
#
# build a table to visualize the survival by passenger class
#
  classes <- length(unique(train_data$Pclass))
  train_table <- data.frame(Survived = numeric(length = classes + 1), 
                            Died = numeric(length = classes + 1))
  rownames(train_table) <- c("First Class", "Second Class", 
                             "Third Class", "Total")
  for (i in 1:length(unique(train_data$Pclass))) {
    train_table[i, 1] <- 
      sum(train_data[train_data$Pclass == i, "Survived"])
    train_table[i, 2] <- 
      nrow(train_data[train_data$Pclass == i & 
                          train_data$Survived == 0, "Survived"])
  }
  train_table[i + 1, 1] <- sum(train_table[1:i, 1])
  train_table[i + 1, 2] <- sum(train_table[1:i, 2])
  train_table[1, 1] <- sum(train_data[train_data$Pclass == 1, "Survived"])
  x <- barplot(t(train_table),
          border = NA, 
          col = c("aquamarine3", "coral2"),
          legend.text = c("Survived", "Died"),
          args.legend = list(x = 2, y = 1.5 * max(train_table)),
          cex.names = 0.75,
          xlim = c(0, 5), 
          space = 0.25,
          ylab = "Count",
          main = "Survival by Ticket Class",
          cex.main = 1.25,
          font.main = 1)
#
# making the labels pretty in the center of the stacked
# bars is messy in base r barplot
#
  for (i in 1:nrow(train_table)) {
    text(x[i],
         c(train_table[i, 1] / 2, train_table[i, 2] / 2 + train_table[i, 1]),
         c(train_table[i, 1], train_table[i, 2]))
  }
#
# create a feature to visualize survival by men, women, children
#
  train_data %>% 
    mutate(MWC = character(length = nrow(train_data))) %>%
    impute_lm(Age ~ Fare + Pclass + Sex + Embarked) %>%
    mutate(Survived = factor(Survived)) %>%
    select(Age, Sex, MWC, Survived) %>%
    mutate(MWC = ifelse(Age < 16, "Child", 
                        ifelse(Sex == "male", "Man", "Woman"))) %>%
    mutate(MWC = factor(MWC, levels = c("Man", "Woman", "Child"))) %>%
    group_by(MWC, Survived) %>%
    count() %>%
    ggplot(aes(x = MWC)) +
    geom_bar(aes(y = n, fill = Survived),
             stat = "identity", 
             alpha = 0.75) +
    labs(title = "Survival by Man/Woman/Child",
         x = "",
         y = "Count") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(labels = c("No", "Yes"),
                      values = c("coral2", "aquamarine3")) +
    theme(legend.title=element_blank())
  
#
# structure data using the shadow frame to use later
# in a comparison of density of missing ages
# note we don't need to shadow the Survived column (as there are no missing)
#
  shadow_train <- 
    bind_shadow(train_data[, - which(colnames(train_data) == "Survived")])
  shadow_test <-
    bind_shadow(test_data)
#
# this shows how using the shadow frame facilitates
# the visualiztion of missing values using the Age_NA column
#
  shadow_train %>%
    ggplot(aes(x = Pclass,
               fill = Age_NA)) + 
    geom_density(alpha = 0.5) +
    labs(title = paste0("density of passenger classes\n",
                        "for training data for cases\n",
                        "having and missing age")) +
    theme(plot.title = element_text(size = 12, hjust = 0.5)) +
    scale_x_continuous(breaks = c(1, 2, 3)) +
    scale_fill_manual(labels = c("Has Age", "Missing"),
                      values = c("coral2", "aquamarine3")) +
    theme(legend.title=element_blank())
  
  shadow_test %>%
    ggplot(aes(x = Pclass,
               fill = Age_NA)) + 
    geom_density(alpha = 0.5) +
    labs(title = paste0("density of passenger classes\n",
                        "for test data for cases\n",
                        "having and missing age")) +
    theme(plot.title = element_text(size = 12, hjust = 0.5)) +
    scale_x_continuous(breaks = c(1, 2, 3)) +
    scale_fill_manual(labels = c("Has Age", "Missing"),
                      values = c("coral2", "aquamarine3")) +
    theme(legend.title=element_blank())
#  
  data_list <- list(train = train_data, test = test_data)
#  
  for (i in 1:length(data_list)) {
#
# normal ggplot
#
    print(ggplot(data = data_list[[i]],
                 aes(x = PassengerId, y = Age)) +
            geom_point() +
            labs(title = paste0("ages for all passengers\n",
                                "for ", names(data_list)[i], " data")) +
            theme(plot.title = element_text(size = 12, hjust = 0.5)) +
            theme(legend.title = element_blank()))
#  
# with added function geom_miss_point
# note use of print() wrapper due to calling ggplot() in a loop
# note controlling colors to always have not missing aquamarine2
# and missing coral2
#
    print(ggplot(data = data_list[[i]],
                 aes(x = PassengerId, y = Age)) +
            geom_miss_point() +
            labs(title = paste0("ages for all passengers\n",
                                "for ", names(data_list)[i], " data")) +
            scale_color_manual(labels = c("Missing", "Has Age"),
                               values = c("coral2", "aquamarine3")) +
            theme(plot.title = element_text(size = 12, hjust = 0.5)) +
            theme(legend.title = element_blank()))
#
# and adding facets
#
    print(ggplot(data = data_list[[i]],
                 aes(x = PassengerId,
                     y = Age)) + 
            geom_miss_point() + 
            facet_wrap(~Pclass, ncol = 2) + 
            theme(legend.position = "bottom") +
            labs(title = paste0("ages for all passengers\n",
                                "for ", names(data_list)[i], " data\n",
                                "by passenger class")) +
            scale_color_manual(labels = c("Missing", "Has Age"),
                              values = c("coral2", "aquamarine3")) +
            theme(plot.title = element_text(size = 12, hjust = 0.5)) +
            theme(legend.title = element_blank()))
#
# imputing on the bound frame keeps track
# of what data have imputed value making
# it easy to visualize
# note here we "cheat" and use the labels to impute
# age for the training data
#
    if ("Survived" %in% colnames(data_list[[i]])) {
      print(data_list[[i]] %>%
              bind_shadow() %>%
              impute_lm(Age ~ Fare + Survived + Pclass + Sex + Embarked) %>%
              ggplot(aes(x = PassengerId,
                         y = Age,
                         color = Age_NA)) + 
              geom_point() +
              labs(title = paste0("ages for all passengers\n",
                                  "for ", names(data_list)[i], " data\n",
                                  "with missing values imputed")) +
              scale_color_manual(labels = c("Has Age", "Missing"),
                                values = c("aquamarine3", "coral2")) +
              theme(legend.title = element_blank()) +
              theme(plot.title = element_text(size = 12, hjust = 0.5)))
    } else {
      print(data_list[[i]] %>%
              bind_shadow() %>%
              impute_lm(Age ~ Fare + Pclass + Sex + Embarked) %>%
              ggplot(aes(x = PassengerId,
                         y = Age,
                         color = Age_NA)) + 
              geom_point() +
              scale_color_manual(labels = c("Has Age", "Missing"),
                                 values = c("aquamarine3", "coral2")) +
              labs(title = paste0("ages for all passengers\n",
                                  "for ", names(data_list)[i], " data\n",
                                  "with missing values imputed")) +
              theme(plot.title = element_text(size = 12, hjust = 0.5))+
              theme(legend.title = element_blank()))
    }
#
# generate shadow data for an upset plot
#
    data_list[[i]] %>%
      as_shadow_upset() %>%
      upset(main.bar.color = "aquamarine3",
            matrix.color = "red", 
            text.scale = 1.5,
            nsets = 3)
    grid.text(paste0(names(data_list)[i], " data\n"),
              x = 0.65, y = 0.9, 
              gp=gpar(fontsize = 12))
#
# visualize missing
#
    print(gg_miss_var(data_list[[i]]) +
            labs(title = paste0("missing data for ", 
                                names(data_list)[i], " data")) +
            theme(plot.title = element_text(size = 12, hjust = 0.5)))
    
    miss_var_summary(data_list[[i]])
#    
    data_list[[i]] %>%
      group_by(Pclass) %>%
      miss_var_summary()
  }