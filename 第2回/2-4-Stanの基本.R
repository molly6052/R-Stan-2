#パッケージの読み込み
library(rstan)
require(rstan)

#計算の高速化
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

#分析対象のデータ
file_beer_sales_1<-read.csv("2-4-1-beer-sales-1.csv")

head(file_beer_sales_1,n=3)

#サンプルサイズ
sample_size<-nrow(file_beer_sales_1)

#listにまとめる
data_list<-list(sales=file_beer_sales_1$sales,N=sample_size)

mcmc_result<-stan(
  file="2-4-1-calc-mean-variance.stan",
  data=data_list,
  seed=1,
  chains=4,
  iter=2000,
  warmup=1000,
  thin=1
)

#結果の表示
print(
  mcmc_result,
  probs=c(0.025,0.5,0.975)
  )

#トレースプロット(バーンイン期間無し)
traceplot(mcmc_result)

#トレースプロット(バーンイン期間あり)
traceplot(mcmc_result,inc_warmup=T)

#MCMCサンプルの抽出
mcmc_sample<-rstan::extract(mcmc_result,permuted=FALSE)
class(mcmc_sample)

#各々の名称
dimnames(mcmc_sample)

mcmc_sample[1,"chain:1","mu"]

#パラメータmuの1回目のチェーンのMCMCサンプル
mcmc_sample[,"chain:1","mu"]

#パラメータmuの1回目のチェーンのMCMCサンプルの個数
length(mcmc_sample[,"chain:1","mu"])

#4つのチェーンすべてのMCMCサンプルの個数
length(mcmc_sample[,,"mu"])

#4つのチェーンがあるので，1000iter×4ChainのMatrix
dim(mcmc_sample[,,"mu"])
class(mcmc_sample[,,"mu"])

#ベクトルにする
mu_mcmc_vec<-as.vector(mcmc_sample[,,"mu"])

#事後中央値
median(mu_mcmc_vec)

#事後期待値
mean(mu_mcmc_vec)

#95%ベイズ信用区間
quantile(mu_mcmc_vec,probs = c(0.025,0.975))

#トレースプロットの描画

library(ggfortify)
autoplot(ts(mcmc_sample[,,"mu"]),
        facets=F,
        ylab="mu",
        main="トレースプロット")

#ggplotによる事後分布の可視化
#データの整形
mu_df<-data.frame(
  mu_mcmc_sample=mu_mcmc_vec
)
#図示
ggplot(data=mu_df,mapping=aes(x=mu_mcmc_sample))+
  geom_density(size=1.5)


if (!require("remotes")) install.packages("remotes")
remotes::install_github('sinhrks/ggfortify')