summary_by_category <- function(df){
    df$one <- rep(1, nrow(df))
    df <- data.table(df)
    setkey(df,category_id)
    result <- as.data.frame(df[,list(count = sum(one),
                               mean_weight = mean(billed_weight), std_dev_weight = sd(billed_weight),
                               min_weight = min(billed_weight), max_weight = max(billed_weight),
                               first_quartile_weight = as.numeric(quantile(billed_weight, 0.25)),
                               median_weight = median(billed_weight),
                               third_quartile_weight = as.numeric(quantile(billed_weight, 0.75)),
                               higher_outlier = higher_outliers(billed_weight),
                               lower_outlier = lower_outliers(billed_weight),
                               mean_weight_mod = mean(rm_outliers(billed_weight)), std_dev_weight_mod = sd(rm_outliers(billed_weight)),
                               min_weight_mod = min(rm_outliers(billed_weight)), max_weight_mod = max(rm_outliers(billed_weight)),
                               first_quartile_weight_mod = as.numeric(quantile(rm_outliers(billed_weight), 0.25)),
                               median_weight_mod = median(rm_outliers(billed_weight)),
                               third_quartile_weight_mod = as.numeric(quantile(rm_outliers(billed_weight), 0.75))
                               ),
                               by=list(category_id)])
    return(result)
}

summary_by_product <- function(df){
    df$one <- rep(1, nrow(df))
    df <- data.table(df)
    setkey(df,product_id)
    result <- as.data.frame(df[,list(count = sum(one),
                                     mean_weight = mean(billed_weight), std_dev_weight = sd(billed_weight), median_weight = median(billed_weight),
                                     higher_outlier = higher_outliers(billed_weight),
                                     lower_outlier = lower_outliers(billed_weight),
                                     mean_weight_mod = mean(rm_outliers(billed_weight)), std_dev_weight_mod = sd(rm_outliers(billed_weight)),
                                     median_weight_mod = median(rm_outliers(billed_weight)),
                                     median_0.1 = observations_in_range(rm_outliers(billed_weight), 0.1),
                                     most_frequent = Mode(data_in_range(rm_outliers(billed_weight), 0.1)),
                                     most_frequent_count = Mode_count(data_in_range(rm_outliers(billed_weight), 0.1))
                                    ),
                                    by=list(product_id)])
    return(result)
}

rm_outliers <- function(vec){
    if(length(vec) == 1){
        return (vec)
    }
    iqrange <- IQR(vec,na.rm = TRUE)
    first_quartile <- as.numeric(quantile(vec, 0.25))
    third_quartile <- as.numeric(quantile(vec, 0.75))
    result <- vec[vec <= (third_quartile + 1.5*iqrange) & vec >= (first_quartile - 1.5*iqrange)]
    return(result)
}

higher_outliers <- function(vec){
    if(length(vec) == 1){
        return (0)
    }
    iqrange <- IQR(vec,na.rm = TRUE)
    third_quartile <- as.numeric(quantile(vec, 0.75))
    result <- vec[vec > (third_quartile + 1.5*iqrange)]
    return(as.double(length(result)))
}

lower_outliers <- function(vec){
    if(length(vec) == 1){
        return (0)
    }
    iqrange <- IQR(vec,na.rm = TRUE)
    first_quartile <- as.numeric(quantile(vec, 0.25))
    result <- vec[vec < (first_quartile - 1.5*iqrange)]
    return(as.double(length(result)))
}

observations_in_range <- function(vec,val){
    if(length(vec) == 1){
        return (1)
    }
    med <- median(vec)
    result <- sum(vec >= (1-val)*(med) & vec <= (1+val)*(med))
    return(as.double(result))
}

data_in_range <- function(vec,val){
    if(length(vec) == 1){
        return (vec)
    }
    med <- median(vec)
    vec <- vec[vec >= (1-val)*(med) & vec <= (1+val)*(med)]
    return(vec)
}

Mode <- function(x) {
    if(length(x) == 0){
        return(x)
    }
    
    if(length(x) == 1){
        return(x)
    }
    tab = as.data.frame(table(x), stringsAsFactors = FALSE)
    return (mean(as.double(tab[tab[,2] == max(tab[,2]),]$x)))
}

Mode_count <- function(x){
    if(length(x) == 0){
        return(0)
    }
    if(length(x) == 1){
        return(1)
    }
    tab = as.data.frame(table(x))
    return (as.double(max(tab[,2])))
}
