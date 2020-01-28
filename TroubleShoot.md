## 问题1

我的电脑的配置是windows10专业版，64位配置，R语言版本是R x64 3.6.1，Rtools用的是Rtools35。     
      我在R语言中使用library(devtools)    install_github("whcsu/SurvELM") 或者require(devtools)  devtools::install_github("whcsu/SurvELM") 中安装"SurvELM"程序包，但无法正常安装程序包，R语言程序报错结果是：Downloading GitHub repo whcsu/SurvELM@master
√  checking for file 'C:\Users\admin\AppData\Local\Temp\RtmpeuANGV\remotescec49ef45cc\whcsu-SurvELM-22c63a8/DESCRIPTION' ...
-  preparing 'SurvELM':
√  checking DESCRIPTION meta-information ... 
-  cleaning src
-  checking for LF line-endings in source and make files and shell scripts
-  checking for empty or unneeded directories
-  building 'SurvELM_0.9.0.tar.gz'
   
ERROR: failed to lock directory 'C:/Program Files/R/R-3.6.1/library' for modifying
Try removing 'C:/Program Files/R/R-3.6.1/library/00LOCK-SurvELM'
Error: Failed to install 'SurvELM' from GitHub:
  (converted from warning) installation of package ‘C:/Users/admin/AppData/Local/Temp/RtmpeuANGV/filececab35d8/SurvELM_0.9.0.tar.gz’ had non-zero exit status
    我们重新安装和测试了R程序，但都无法通过install_github("whcsu/SurvELM")  和 devtools::install_github("whcsu/SurvELM") 安装"SurvELM"程序包。我们也重新安装了Rcpp,survival,RcppNumerical,glmnet,CoxBoos，RcppArmadillo,RcppEigen, RcppNumericalt程序包，结果仍然是上面的报错内容。
    特向您求助，希望您能够帮助我们解决通过install_github("whcsu/SurvELM")  和 devtools::install_github("whcsu/SurvELM") 安装"SurvELM"程序包的难题。
    
    ## 解答1
    
1、手工找到删除  'C:/Program Files/R/R-3.6.1/library/00LOCK-SurvELM'文件，重试。

2、据我所知，有些包对Windows 10的R主程序的安装位置命名有要求：安装R路径中不能有空格，你好像是安装在'C:/Program Files/这里Program和Files之间有空格，可能导致出错。更改R的安装位置（路径中不能有空格），重试。

3、Rtools版本最好用Rtools34, Rtools35版本我也发现有些问题。替换Rtools版本重试。

可以先从第一步入手，第一步不行，再进行第二步，依次类推。
