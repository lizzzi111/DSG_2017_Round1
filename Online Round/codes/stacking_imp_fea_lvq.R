library(e1071)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(real~., data=tr, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


lvq_bf_fit = predict(fullmod, newdata = ts, type = "response" )
auc(ts$real, lvq_bf_fit )

mm <- train(real~., data=tr, method="lvq", preProcess="scale")

impo_fea = names(tr)[importance$importance$X0>0.7]

bv = glm(real~., tr[,impo_fea], family="binomial")
bv_fit = predict(bv, ts, type="response")
auc(ts$real, bv_fit )

glm_fit = glm(real~., full[,impo_fea], family = "binomial")
unknown$is_listened = predict(glm_fit, newdata = unknown, type = "response" )
write.csv(unknown[,c("sample_id", "is_listened")], "./pred_unknown/stacking_glm_factorization_imp_fea.csv", row.names = F)
