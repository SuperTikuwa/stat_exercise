library(dplyr)

# csv読み込み
data <- read.csv("./t.csv",header = T,row.names = 1)
data


# 読み込んだcsvに，各地域におけるYes,Noの合計値の列を追加
# また，dataframeからtibbleへの変換もおこなっている
data <- data |> 
  as_tibble() |>
  rowwise() |>
  mutate( Sum = Yes+No)
bak<-data
# 作成したテーブルにYes,Noの合計値の行を追加
data <- 
  add_row(
    data,
    Yes=sum(data$Yes),
    No=sum(data$No),
    Sum=sum(data$Sum)
    )

# 期待度数を計算するためのテーブルを作成
expectedData <- data

# 期待度数を計算
expectedData <- expectedData|>
  as_tibble()|>
  rowwise()|>
  mutate(
    Yes = Sum*(expectedData[6,]$Yes/expectedData[6,]$Sum),
    No = Sum*(expectedData[6,]$No/expectedData[6,]$Sum)
    )

# 不要な行の削除
expectedData <- expectedData[-6,]
expectedData <- expectedData[-3]

# 計算しやすいようにデータを整形
newData <- tibble(
  Yes = data$Yes,
  No = data$No,
  ExpectedYes = expectedData$Yes,
  ExpectedNo = expectedData$No,
  Sum = data$Sum
)
newData

# カイ二乗値を計算
stat <- newData |>
  rowwise() |>
  mutate(
    ChiYes = ((Yes-ExpectedYes)^2/ExpectedYes),
    ChiNo = ((No-ExpectedNo)^2/ExpectedNo)
    )
stat
stat <- sum(stat$ChiYes) + sum(stat$ChiNo)
stat


chisq.test(bak)
chisq.test(expectedData)
