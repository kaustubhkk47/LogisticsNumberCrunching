## To summarise by dates and LMD merchants


preprocess <- function(df){
    df$NonCharged <- ifelse(df$`SUM(merchant_logistics_income)`==0, 1, 0)
    df$NonChargedNonLMD <- as.integer(df$NonCharged & df$lmd_enabled)
    df$Date <- format(as.Date(df$created_at),'%m/%d/%Y')
    return (df)
}

summarise_by_date <- function(df){
    df <- data.table(df)
    setkey(df,Date, NonCharged,lmd_enabled)
    result <- as.data.frame(df[,list(qty = sum(qty), gmv = sum(gmv), cashback = sum(cb)
                                ,commission = sum(commission),
                                MerchantLogisticsIncome = sum(merchant_logistic_income),
                                LogisticsCharge = sum(logistic_charge),
                                CustomerLogisticsIncome = sum(customer_logistic_income)), by = list(Date, NonCharged,lmd_enabled)])
    
    return (result)
}

query <- "SELECT soi.id,soi.order_id,so.created_at, soi.product_id, soi.merchant_id,
ROUND(SUM(t1.pg_commission+mp_commission)*100/(SUM(soi.selling_price)/1000),2) AS 
commission_perentage,
COUNT(DISTINCT so.id) AS order_count, SUM(soi.qty_ordered) qty, 
SUM(soi.selling_price)/1000 gmv, IFNULL(SUM(pu.amount),0)/1000 cb, 
SUM(t1.pg_commission+mp_commission) AS commission,SUM(merchant_logistics_income),
SUM(IFNULL(CASE WHEN m.lmd_enabled = 0 THEN 0 ELSE IFNULL((soi.qty_ordered * lc.avg_item_freight),(dcf.avg_item_freight*soi.qty_ordered )) END,0)) logistic_charge, 
SUM(mp_commission),SUM(t1.pg_commission),SUM(soi.shipping_amount)/1000 AS 
customer_logistic_income FROM (SELECT sd.order_id,sd.order_item_id, 
SUM(IF(commission_meta_id = 2,sor.amount,0))/1000 AS mp_commission,
SUM(IF(commission_meta_id = 3,sor.amount,0))/1000 AS pg_commission,
SUM(IF(commission_meta_id = 4,sor.amount,0))/1000 AS merchant_logistics_income 
FROM payouts.sales_data sd JOIN payouts.sales_order_revenue sor ON 
sor.direction = 1 AND sd.order_item_id = sor.order_item_id 
WHERE 
sd.created_at BETWEEN '2015-11-01' AND '2015-11-30 23:59:59' GROUP BY 1,2)t1 
JOIN mktplace.sales_order so ON so.id = t1.order_id AND so.payment_status=2 
JOIN mktplace.sales_order_item soi ON soi.id = t1.order_item_id 
LEFT JOIN code.promocode_usage pu ON pu.order_item_id = soi.id 
JOIN mktplace.catalog_product cp ON cp.id = soi.product_id 
LEFT JOIN logistics.productwise_freight lc ON lc.product_id = soi.product_id and lc.month = 11
LEFT JOIN logistics.categorywise_freight dcf ON (dcf.category_id = cp.category_id)
LEFT JOIN mktplace.merchant m ON (m.id = cp.merchant_id) GROUP BY 1,2;"