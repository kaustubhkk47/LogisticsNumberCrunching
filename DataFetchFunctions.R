get_all_data <- function(){
    host = "54.255.153.194"
    user = "kaustubh_14639"
    password = "Qwerty0987"
    port = 9078
    dwa_dev = dbConnect(MySQL(), username= user, password=password, dbname = "dwa_dev", host=host, port = port)
    
    logquery = "select billed_weight from sales_order_item_freight_new where product_id = '1703104';"
    
    logres = dbSendQuery(dwa_dev, logquery)
    logdetails = as.data.frame(fetch(logres, n = -1))
    dbDisconnect(dwa_dev)
    return (logdetails)
}

get_product_names <- function(){
    host = "54.255.153.194"
    user = "kaustubh_14639"
    password = "Qwerty0987"
    port = 9078
    mktplace_catalog = dbConnect(MySQL(), username= user, password=password, dbname = "mktplace_catalog", host=host, port = port)
    
    
    #product_ids <- paste('(', str_c(vec, collapse = ','), ')',sep = '')
    
    #product_name_query = paste("select id as product_id,name,price,brand from catalog_product where id in ", product_ids, sep =' ')
    product_name_query = "select id as product_id,name,price,brand from catalog_product"
    
    product_name_res = dbSendQuery(mktplace_catalog, product_name_query)
    
    product_name_details = as.data.frame(fetch(product_name_res, n = -1))
    
    dbDisconnect(mktplace_catalog)
    
    return (product_name_details)
}

close_all_connections <- function(df){
    all_cons <- dbListConnections(MySQL())
    for (con in all_cons){
        dbDisconnect(con)
    }
}