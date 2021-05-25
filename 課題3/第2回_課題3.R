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
sample_size<-nrow(file_beer_sales_1)   #nrowでサンプルの行数を取得

#listにまとめる
data_list<-list(sales=file_beer_sales_1$sales,N=sample_size) #サンプルの売上行から売り上げを取得.

mcmc_result<-stan(
  file="2-4-1-calc-mean-variance.stan", #
  data=data_list,
  seed=1,                              #処理の間は乱数の値を保持
  chains=4,                            #乱数生成のセット数
  iter=2000,　　　　　　　　　　　　   #生成する乱数の数
  warmup=1000,　　　　　　　　　　　　 #バーイン期間,初期値を含んだ1000個を切り捨てる
  thin=1                               #乱数の間引き，一つの乱数につき一つを採用
)                                      #生成されて乱数を順番通りに採用


#トレースプロット(バーンイン期間無し)
traceplot(mcmc_result)                 #初期値を含むデータ2000個

#トレースプロット(バーンイン期間あり)
traceplot(mcmc_result,inc_warmup=T)    #データ1000個


#ベクトルにする
mu_mcmc_vec<-as.vector(mcmc_sample[,,"mu"]) #4つのチェーンすべてがmcmc_sampleに行列として格納されている. P.126
                                            #as.vectorで4000の要素を持つベクトルに変換.
#データの整形
mu_df<-data.frame(                          #データフレームにmu_mcmc_sample=mu_mcmc_vecとして格納
  mu_mcmc_sample=mu_mcmc_vec
)
#図示
ggplot(data=mu_df,mapping=aes(x=mu_mcmc_sample))+ #dataにデータフレームに格納した乱数,X軸にサンプル数,線の太さを1.5
  geom_density(size=1.5)
