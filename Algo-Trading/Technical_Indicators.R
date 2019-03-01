                            # We have implemented three indicators MACD,CMF and RSI 




# Read the files from folder and merge into a single frame

filename <- list.files(path="C:/Users/Nilumaa's Saurav/Desktop/Sem8/Financial Analytics Lab/Algo-Trading/Data")
combine_data = as.data.frame(lapply(filename, function(x) read.csv(file=x)))


# MACD implementaion
Adj_close_A = combine_data[,c("Adj.Close","Adj.Close.1","Adj.Close.2","Adj.Close.3","Adj.Close.4","Adj.Close.5","Adj.Close.6","Adj.Close.7","Adj.Close.8","Adj.Close.9")]
n = ceiling(0.8*length(Adj_close_A[,1]))

lSMA = 50
lFMA = 10

SMA = as.data.frame(matrix(data = NA,nrow = nrow(Adj_close_A),ncol = ncol(Adj_close_A)))

i = lSMA
while(i <= nrow(Adj_close_A)){
  iS = (i - lSMA +1):i
  SMA[i,] = colMeans(Adj_close_A[iS,])
  i = i + 1
}

j = lFMA
FMA = as.data.frame(matrix(data = NA,nrow = nrow(Adj_close_A),ncol = ncol(Adj_close_A)))
while(j <= nrow(Adj_close_A)){
  iF = (j - lFMA +1):j
  FMA[j,] = colMeans(Adj_close_A[iF,])
  j = j + 1
}

MACD = FMA - SMA

#CAF implementation

high = combine_data[,c("High","High.1","High.2","High.3","High.4","High.5","High.6","High.7","High.8","High.9")]
low = combine_data[,c("Low","Low.1","Low.2","Low.3","Low.4","Low.5","Low.6","Low.7","Low.8","Low.9")]
vol = combine_data[,c("Volume","Volume.1","Volume.2","Volume.3","Volume.4","Volume.5","Volume.6","Volume.7","Volume.8","Volume.9")]
close =combine_data[,c("Close","Close.1","Close.2","Close.3","Close.4","Close.5","Close.6","Close.7","Close.8","Close.9")]

# clean the high,low, vol and close data
for(i in 1:ncol(high)){
  high[,i] = as.numeric(as.character(high[,i]))
  low[,i] = as.numeric(as.character(low[,i]))
  vol[,i] = as.numeric(as.character(vol[,i]))
  close[,i] = as.numeric(as.character(close[,i]))
}

high[which(is.na(high$High)),] = colMeans(na.omit(high))
vol[which(is.na(vol$Volume)),] = colMeans(na.omit(vol))
low[which(is.na(low$Low)),] = colMeans(na.omit(low))
close[which(is.na(close$Close)),] = colMeans(na.omit(close))
#CAF Calculation

mult = (((close - low) - (high - close))/(high - low))*vol

k = 50
CMF = as.data.frame(matrix(data = NA,nrow = nrow(Adj_close_A),ncol = ncol(Adj_close_A)))
while(k <= nrow(mult)){
  iM = (k - 50 +1):k
  CMF[k,] = colSums(mult[iM,])/colSums(vol[iM,])
  k = k + 1
}
                          #RSI indicator based on past 14 days calculation

logreturn = log(Adj_close_A[2:nrow(Adj_close_A),1:ncol(Adj_close_A)])-log(Adj_close_A[1:(nrow(Adj_close_A)-1),1:ncol(Adj_close_A)])
Gain = as.data.frame(matrix(data = 0, nrow = nrow(logreturn),ncol = ncol(logreturn)))
Loss =  as.data.frame(matrix(data = 0, nrow = (nrow(logreturn)),ncol = ncol(logreturn)))                     

for(i in 1:ncol(logreturn))
{
  for(j in 1:nrow(logreturn))
  {
    if(logreturn[j,i]>=0)
      {
        Gain[j,i]=logreturn[j,i]
      }
    else
      Loss[j,i] = logreturn[j,i]
    
  }
}

Avggain = as.data.frame(matrix(data = 0, nrow = (nrow(logreturn)+1),ncol = ncol(logreturn)))
Avgloss =  as.data.frame(matrix(data = 0, nrow = (nrow(logreturn)+1),ncol = ncol(logreturn)))

period = 14
k = period
while(k<=nrow(logreturn))
{
  is = (k-period+1):k
  Avggain[k+1,] = colMeans(Gain[is,])
  Avgloss[k+1,] = colMeans(Loss[is,])
  
  k = k+1
}

RS = as.data.frame(matrix(data = NA, nrow = (nrow(logreturn)+1),ncol = ncol(logreturn)))
RS[(period+1):nrow(RSI),] = -(Avggain[(period+1):nrow(RSI),])/(Avgloss[(period+1):nrow(RSI),])

RSI = as.data.frame(matrix(data = NA, nrow = (nrow(logreturn)+1),ncol = ncol(logreturn)))
RSI[(period+1):nrow(RSI),] = 100*RS[(period+1):nrow(RSI),]/(1+RS[(period+1):nrow(RSI),])


                                     # Now we have RSI,MACD and CMF
                                    #Construct singnals to buy/sell
                             #1 means buy and 0 means hold and -1 means sell


#MACD_signal from 50 days ownwards of selected dataset
# for MACD if it is > 0 then buy and vise-versa
macd_signal = as.data.frame(matrix(data = NA,nrow = nrow(Adj_close_A),ncol = ncol(Adj_close_A)))
for(i in lSMA:(nrow(MACD)))
{
  for(j in 1:ncol(MACD))
  {
    if(MACD[i,j]>0)
    {
      macd_signal[i,j]=1
    }
    else
      macd_signal[i,j] =-1
  }
}
#CMF_signal from 50 days ownwards of selected dataset
# for CMF if it is > 0 then buy(1) and vise-versa

cmf_signal = as.data.frame(matrix(data = NA,nrow = nrow(Adj_close_A),ncol = ncol(Adj_close_A)))

for(i in lSMA:(nrow(CMF)))
{
  for(j in 1:ncol(CMF))
  {
    if(CMF[i,j]>0)
    {
      cmf_signal[i,j]=1
    }
    else
      cmf_signal[i,j] =-1
  }
}

#RSI_signal from 50 days ownwards of selected dataset
# for RSI if it is > 70 then sell and if it is < 30 then buy

rsi_signal = as.data.frame(matrix(data = NA,nrow = nrow(Adj_close_A),ncol = ncol(Adj_close_A)))

for(i in lSMA:(nrow(Adj_close_A)))
{
  for(j in 1:ncol(RSI))
  {
    if(RSI[i,j]>=70)
    {
      rsi_signal[i,j]=-1
    }
    else{
      if(RSI[i,j]<=30)
        rsi_signal[i,j] =1
      else
        rsi_signal[i,j] =0
    }
  }
}

write.csv(rsi_signal,"RSI_SIGNAL.csv")
write.csv(cmf_signal,"CMF_SIGNAL.csv")
write.csv(macd_signal,"MACD_SIGNAL.csv")
 # NOw we have individual signals of each technical indicators
# Best logic is when all three indicators give signal to buy then buy and vice-versa

# But to be little less conservative we assumed that
# When sum of 3 logic give -3 or -2 then sell and if +3,+2 then buy and for any other value(-1,0,+1) don't take any action
Final_signal = macd_signal+cmf_signal+rsi_signal
write.csv(Final_signal,"COMBINE_SIGNAL.csv")
