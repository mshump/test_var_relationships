
# load data and select columns to evaluate
attach(mtcars)
data_in <- mtcars[,c("am","cyl","hp","wt")]


## establish data frame, response variable, independent variables
df_eval <- data.frame()
resp_var <- c("am") #class
pred_vars <- unlist(colnames(data_in[,!(colnames(data_in) %in% resp_var)])) # c("a","b","c")



# begin iteration through independent variables to evaluate relationship to response

for(i in 1:length(pred_vars)) {
  
## Make formula function
Formula <- formula(paste(resp_var,"~ ",  pred_vars[i]))

## fit with logistic
model_out = glm(formula = Formula, data = data_in, family = binomial)

## save evaluation data
resp_var_name <- resp_var
pred_var_name <- pred_vars[i]
est <- summary(model_out)$coefficients[2,1]
p_val <- summary(model_out)$coefficients[2,4]
aic <- model_out$aic

## combine into row of data frame
pred_var_eval <- as.data.frame(cbind(resp_var_name=resp_var_name
      ,pred_var_name=pred_var_name
      ,est=est
      ,p_val=p_val
      ,aic=aic))

## append to full evaluation data frame
df_eval <- rbind(df_eval,pred_var_eval)

}

## print data frame
df_eval