
#----------------------------------------------
# ρ=Oa-b/Ob* Σ((D-Δd)/D)/ Oa-b #
# Please run the source code of the "RandomForestExplainer" package before running the following code #
# https://mirror.lzu.edu.cn/CRAN/src/contrib/randomForestExplainer_0.10.1.tar.gz #
# min_depth_distribution.R  ###  measure_importance.R  ##

#----------------------------------------------
# Max depth
#----------------------------------------------

calculate_tree_depth_single <- function(frame){
  if(!all(c("right daughter", "left daughter") %in% names(frame))){
    stop("The data frame has to contain columns called 'right daughter' and 'left daughter'!
          It should be a product of the function getTree(..., labelVar = T).")
  }
  frame$depth <- NA
  frame$depth[1] <- 0
  for(i in 2:nrow(frame)){
    frame[i, "depth"] <-
      frame[frame[, "left daughter"] == as.numeric(rownames(frame[i,])) |
              frame[, "right daughter"] == as.numeric(rownames(frame[i,])), "depth"] + 1
  }
  return(frame)
}


calculate_max_depth <- function(forest,k){
  depth_frame<-randomForest::getTree(forest, k , labelVar = T) %>%
    mutate_if(is.factor, as.character) %>%
    calculate_tree_depth_single()
  max_depth<-max(depth_frame['depth'])
  return(max_depth)
}

#----------------------------------------------
# conditional depth -- Δd
#----------------------------------------------

conditional_depth <- function(frame, vars){
  `.SD` <- NULL; depth <- NULL; `split var` <- NULL
  index <- data.table::as.data.table(frame)[, .SD[which.min(depth), "number"], by = `split var`]
  index <- index[!is.na(index$`split var`), ]
  if(any(index$`split var` %in% vars)){
    for(j in vars){
      begin <- as.numeric(index[index$`split var` == j, "number"])
      if(!is.na(begin)){
        df <- frame[begin:nrow(frame), setdiff(names(frame), setdiff(vars, j))]
        df[[j]][1] <- 0
        for(k in 2:nrow(df)){
          if(length(df[df[, "left daughter"] == as.numeric(df[k, "number"]) |
                       df[, "right daughter"] == as.numeric(df[k, "number"]), j]) != 0){
            df[k, j] <-
              df[df[, "left daughter"] == as.numeric(df[k, "number"]) |
                   df[, "right daughter"] == as.numeric(df[k, "number"]), j] + 1
          }
        }
        frame[begin:nrow(frame), setdiff(names(frame), setdiff(vars, j))] <- df
      }
    }
  }
  frame[frame == 0] <- NA
  return(frame)
}

min_depth_interactions_values_single <- function(forest, k,vars){
  `.` <- NULL; .SD <- NULL; tree <- NULL; `split var` <- NULL
  interactions_frame <-
    randomForest::getTree(forest, k, labelVar = T) %>%
    mutate_if(is.factor, as.character) %>%
    calculate_tree_depth() %>% cbind(., tree = k, number = 1:nrow(.)) %>%
    as.data.frame()
  interactions_frame[vars] <- as.numeric(NA)
  interactions_frame <-
    data.table::as.data.table(interactions_frame)[, conditional_depth(as.data.frame(.SD), vars), by = tree] %>% as.data.frame()
  mean_tree_depth <- dplyr::group_by(interactions_frame[, c("tree", vars)], tree) %>%
    dplyr::summarize_at(vars, funs(max(., na.rm = TRUE))) %>% as.data.frame()
  mean_tree_depth[mean_tree_depth == -Inf] <- NA
  mean_tree_depth <- colMeans(mean_tree_depth[, vars, drop = FALSE], na.rm = TRUE)
  min_depth_interactions_frame <-
    interactions_frame %>% dplyr::group_by(tree, `split var`) %>%
    dplyr::summarize_at(vars, funs(min(., na.rm = TRUE))) %>% as.data.frame()
  min_depth_interactions_frame[min_depth_interactions_frame == Inf] <- NA
  min_depth_interactions_frame <- min_depth_interactions_frame[!is.na(min_depth_interactions_frame$`split var`), ]
  colnames(min_depth_interactions_frame)[2] <- "variable"
  min_depth_interactions_frame[, -c(1:2)] <- min_depth_interactions_frame[, -c(1:2)] - 1
  return(list(min_depth_interactions_frame, mean_tree_depth))
}

min_depth_interactions_single<- function(forest,k, vars = important_variables(measure_importance(forest)),
                                         mean_sample = "top_trees", uncond_mean_sample = mean_sample){
  variable <- NULL; `.` <- NULL; tree <- NULL; `split var` <- NULL; depth <- NULL
  min_depth_interactions_frame <- min_depth_interactions_values_single(forest, k,vars)
  mean_tree_depth <- min_depth_interactions_frame[[2]]
  min_depth_interactions_frame <- min_depth_interactions_frame[[1]]
  #Y#
  interactions_frame <-  min_depth_interactions_frame [-1]
  #interactions_frame <-
  #min_depth_interactions_frame %>% dplyr::group_by(variable) %>%
  #dplyr::summarize_at(vars, funs(mean(., na.rm = TRUE))) %>% as.data.frame()
  #interactions_frame[is.na(as.matrix(interactions_frame))] <- NA
  
  occurrences <-
    min_depth_interactions_frame %>% dplyr::group_by(variable) %>%
    dplyr::summarize_at(vars, funs(sum(!is.na(.)))) %>% as.data.frame()
  if(mean_sample == "all_trees"){
    non_occurrences <- occurrences
    non_occurrences[, -1] <- 1 - occurrences[, -1]
    interactions_frame[is.na(as.matrix(interactions_frame))] <- 0
    interactions_frame[, -1] <- (interactions_frame[, -1] * occurrences[, -1] +
                                   as.matrix(non_occurrences[, -1]) %*% diag(mean_tree_depth, nrow = length(mean_tree_depth)))
  } else if(mean_sample == "top_trees"){
    non_occurrences <- occurrences
    non_occurrences[, -1] <- forest$ntree - occurrences[, -1]
    minimum_non_occurrences <- min(non_occurrences[, -1])
    non_occurrences[, -1] <- non_occurrences[, -1] - minimum_non_occurrences
    interactions_frame[is.na(as.matrix(interactions_frame))] <- 0
    interactions_frame[, -1] <- (interactions_frame[, -1] * occurrences[, -1] +
                                   as.matrix(non_occurrences[, -1]) %*% diag(mean_tree_depth, nrow = length(mean_tree_depth)))/(forest$ntree - minimum_non_occurrences)
  }
  interactions_frame <- reshape2::melt(interactions_frame, id.vars = "variable")
  colnames(interactions_frame)[2:3] <- c("root_variable", "mean_min_depth")
  occurrences <- reshape2::melt(occurrences, id.vars = "variable")
  colnames(occurrences)[2:3] <- c("root_variable", "occurrences")
  interactions_frame <- merge(interactions_frame, occurrences)
  interactions_frame$interaction <- paste(interactions_frame$root_variable, interactions_frame$variable, sep = ":")
  forest_table <-
    randomForest::getTree(forest, k, labelVar = T) %>%
    mutate_if(is.factor, as.character) %>%
    calculate_tree_depth()%>%cbind(tree = k)
  #Y#
  min_depth_frame <-aggregate(forest_table, by= list(forest_table$'split var'), FUN = min)[c(1,8)]
  ##min_depth_frame <-cbind(tree=k,min_depth_frame)
  #min_depth_frame <- dplyr::group_by(forest_table, tree,  `split var`) %>%
  #  dplyr::summarize(min(depth))
  # colnames(min_depth_frame) <- c("tree", "variable", "minimal_depth")
  #Y#
  colnames(min_depth_frame) <- c( "variable", "uncond_mean_min_depth")
  #min_depth_frame <- as.data.frame(min_depth_frame[!is.na(min_depth_frame$variable),])
  #importance_frame <- get_min_depth_means(min_depth_frame, min_depth_count(min_depth_frame), uncond_mean_sample)
  #colnames(importance_frame)[2] <- "uncond_mean_min_depth"
  interactions_frame <- merge(interactions_frame, min_depth_frame)
}

calculate_rD<-function(D,frame,num.tree){
  rD<-(D-frame[3])*frame[4]/D
  colnames(rD)<-paste0('tree',num.tree)
  return(rD)
}