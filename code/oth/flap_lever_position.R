library(tidyverse)

# 変化点の探し方
path <- "data/flap_lever/20201202_0012.csv"
df <- read_csv(path) %>% 
  mutate(shift_Flap_lever = lag(Flap_lever ), # lag列（1行ずらしの列）
         diff = Flap_lever - shift_Flap_lever) # position変化量の列ができる

# 変化点を見つける、とかなら差分列を作る上の方法が一般的。
# 今回のようなケースであれば、0のFlap_leverはいらないぽいので、
# 最初から0を削除してしまうのも手

filter_num <- 264 
  
df <- read_csv(path) %>% 
  filter(Flap_lever != 0) %>%  # 0は削除
  # Positionがfilter_numより大きい場合はF
  # そうでない場合はTになるflag列を作る
  mutate(flag = case_when(Position >= filter_num ~ F,
                          T ~ T)) 

# あとはTの行数を数えるなどで秒数は算出可
df %>% 
  filter(flag == T) %>%
  nrow()


# グラフの作り方
path <- "data/flap_lever/"
files <- list.files(path, full.names = T)

all_df <- data.frame() # 全部のデータを入れる
for (i in 1:length(files)){
  
  df <- read_csv(files[i]) %>% 
    filter(Flap_lever != 0) %>%  # 0は削除
    mutate(flag = case_when(Position >= filter_num ~ F,
                            T ~ T),
           file = str_extract(files[i], "\\d{8}_\\d{4}") # ファイル判別用の列
           ) %>% 
    mutate(second = 0:(nrow(.)-1) # 経過時間の列
           ) 
  
  all_df <- bind_rows(all_df, df) # 全データ結合

}


# 横軸経過時間、縦軸positionのグラフを作る
ggplot(all_df) +
  geom_line(aes(x = second, y = Position, color = file))

