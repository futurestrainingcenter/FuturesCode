library(shinylive)
library(httpuv)

shinylive::export(
  appdir = "/Users/watts/Documents/Futures Performance Center/FuturesApps/Leaderboards",
  destdir = "/Users/watts/Documents/Futures Performance Center/FuturesApps/docs"
)

httpuv::runStaticServer(dir = "/Users/watts/Documents/Futures Performance Center/FuturesApps/docs", port = 8888)