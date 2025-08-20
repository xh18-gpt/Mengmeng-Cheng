drop if id == 341400
drop if id == 371200
drop if id == 460300



//命令安装

 ssc install xxx , replace
//批量插值

global X 年末金融机构各项贷款余额万元 年末金融机构存款余额万元 金融机构存贷款余额 地区生产总值万元 金融机构存贷款余额与GDP比值 固定资产投资总额万元 户籍人口万人 人均固定资产 社会消费品零售总额万元 人均社会销售品零售总额 年末单位从业人员数万人 就业人数 就业率 医院卫生院床位数张 每万人医疗卫生机构床位 户籍人口万人2 普通高等学校在校学生数人 每万人中普通高等学校在校学生数 行政区域土地面积平方公里 面积1 人口密度 在岗职工工资总额万元 公共图书馆图书总藏量千册件 每百人图书馆藏书数 全年用电量万千瓦时 人均用电 人均日生活用水量_升 人均道路面积_平方米 国际互联网用户数户 城市排水和污水处理情况排水管道长度_公里 排水管道密度 建成区面积_平方公里 人均居住面积 城区人口_万人 第二产业增加值万元 第三产业增加值万元 第三产业比第二产业 第三产业占比 二三产业占比 内资企业工业总产值万元 外商投资企业工业总产值万元 内资比外资企业 专利授权数全件 发明全件 年末实有公共汽电车营运车辆数辆 全年公共汽电车客运总量万人次 年末实有巡游出租汽车营运车数辆 电信业务收入万元 移动电话年末用户数万户 互联网宽带接入用户数万户
sort 年份 行政区划代码
foreach var of global X {
    gen y =  `var'
    by 行政区划代码 :ipolate y 年份,gen(`var'_1) epolate
    drop y
}

global Y 年末金融机构各项贷款余额万元_1 年末金融机构存款余额万元_1 金融机构存贷款余额j_1 地区生产总值万元_1 金融机构存贷款余额与GDP比值_1 固定资产投资总额万元_1 户籍人口万人_1 人均固定资产_1 社会消费品零售总额万元_1 人均社会销售品零售总额_1 年末单位从业人员数万人_1 就业人数_1 就业率_1 医院卫生院床位数张_1 每万人医疗卫生机构床位_1 户籍人口万人2_1 普通高等学校在校学生数人_1 每万人中普通高等学校在校学生数_1 行政区域土地面积平方公里_1 面积1_1 人口密度_1 在岗职工工资总额万元_1 公共图书馆图书总藏量千册件_1 每百人图书馆藏书数_1 全年用电量万千瓦时_1 人均用电_1 人均日生活用水量_升_1 人均道路面积_平方米_1 国际互联网用户数户_1 城市排水和污水处理情况排水管道长度_公里_1 排水管道密度_1 建成区面积_平方公里_1 人均居住面积_1 城区人口_万人_1 第二产业增加值万元_1 第三产业增加值万元_1 第三产业比第二产业_1 第三产业占比_1 二三产业占比_1 内资企业工业总产值万元_1 外商投资企业工业总产值万元_1 内资比外资企业_1 专利授权数全件_1 发明全件_1 年末实有公共汽电车营运车辆数辆_1 全年公共汽电车客运总量万人次_1 年末实有巡游出租汽车营运车数辆_1 电信业务收入万元_1 移动电话年末用户数万户_1 互联网宽带接入用户数万户_1

sort 年份 地区生产总值万元
foreach var of global Y {
    gen y =  `var'
    by 年份 :ipolate y 地区生产总值万元,gen(`var'_2) epolate
    drop y
}

sum 




xtset id year

reghdfe score 智慧城市试点 , absorb(id year) vce(robust)

reghdfe score 智慧城市试点 地区人口数陆地面积1  外商投资 互联网用户 , absorb(id year) vce(robust)



//回归11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
xtset id year 

reghdfe score_w 智慧城市试点 , absorb(id year)  vce(cluster id)
est store fe1
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w, absorb(id year)  vce(cluster id)
est store fe11
esttab fe1 fe11 using did基准回归结果.rtf, replace b(%12.3f) se(%12.3f) nogap compress s(N r2 r2_a)star(* 0.1 ** 0.05 *** 0.01)
 

//平行趋势检验
clear
//set obs 5331 //设置5个个体
//set seed 10101
//gen id=_n //生成个体id
//expand  //观察值扩展50倍
//drop in 1/5 //删除前五个观察值
//bysort id: gen time=_n+1999 //时间从2000-2048年，共49期gen D=rbinomial(1,0.4)
//gen x1=rnormal(1,7)
tsset id time
//设定多期 DID 模型
//forvalues i=1/6{ gen L`i'_x=L`i'.x }
//bys id: gen y0=5+1*x+ rnormal()
//bys id: gen y1=100+5*x+90*L1_x+90*L2_x+120*L3_x+100*L4_x+90*L5_x +90*L6_x + rnormal()
//gen A=6*x+rnormal()
//replace D=1 if A>=15
//replace D=0 if A<15
//gen y=y0+D*(y1-y0)
//模型估计
reghdfe score 智慧城市试点 人口密度_1_2 二三产业占比_1_2 内资比外资企业_1_2 , absorb(id year) vce(robust)

tvdiff y D x , model(fe) pre(3) post(3) vce(robust) test_tt graph save_graph(mygraph) //一步平行趋势检验
 
//缩尾
foreach i of varlist  score 智慧城市试点  互联网占比  内资比外资企业_1_2   第三产业比第二产业_1_2 就业率 固定资产投资占比{
winsor `i',gen (`i'_w) p(0.01)
}


//平行趋势检验画图
by id : egen time = sum(智慧城市试点)

gen action = 2012 if time ==11
replace action = 2013 if time ==10
replace action = 2014 if time ==9
gen pd = year - action

forvalues i = 12(-1)1{
	gen pre_`i'=(pd==-`i'& treat==1)
}
gen current =(pd==0&treat==1)
forvalues j = 1(1)10{
	gen las_`j'=(pd==`j'& treat==1)
}
//回归
//画图
clear
 use "/Users/knight/Desktop/智慧城市与城市韧性/重新实证/实证表.dta" 
 xtset id year
reg score_w pre_12 pre_11 pre_10 pre_9 pre_8 pre_7 pre_6 pre_5 pre_4 pre_3 pre_2  current las_1 las_2 las_3 las_4 las_5 las_6 las_7 las_8 las_9 las_10 pre_1 互联网占比_w   第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w i.year,  vce(cluster id)
//pre_1放到最后
coefplot,baselevels omitted keep(pre* current las*) vertical recast(connect) order(pre_12 pre_11 pre_10 pre_9 pre_8 pre_7 pre_6 pre_5 pre_4 pre_3 pre_2 pre_1 current las_1 las_2 las_3 las_4 las_5 las_6 las_7 las_8 las_9 las_10 ) yline(0,lp(solid) lc(black)) xline(13,lp(solid)) ytitle("政策效应") xtitle("政策时点") xlabel(1 "-12" 2 "-11" 3 "-10" 4 "-9" 5 "-8" 6 "-7" 7 "-6" 8 "-5" 9 "-4" 10 "-3" 11 "-2" 12 "-1" 13 "0" 14 "1" 15 "2" 16 "3" 17 "4" 18 "5" 19 "6" 20 "7" 21 "8" 22 "9" 23 "10") ciopts(recast(rcap) lc(black) lp(dash) lw(thin)) scale(1.0) name(平行趋势图_graph, replace)
//pre_1 按顺序放
graph export "平行趋势图_graph.jpg", as(jpg) replace

// pre_12 pre_11 pre_10 pre_9 pre_8 pre_7 pre_6 pre_5 pre_4 pre_3 pre_2 pre_1 current las_1 las_2 las_3 las_4 las_5 las_6 las_7 las_8 las_9 las_10

//画图2
 xtset id year
reg score_w  pre_10 pre_9 pre_8 pre_7 pre_6 pre_5 pre_4 pre_3 pre_2  current las_1 las_2 las_3 las_4 las_5 las_6 las_7 las_8 las_9 las_10 pre_1 互联网占比_w   第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w i.year,  vce(cluster id)
//pre_1放到最后
coefplot,baselevels omitted keep(pre* current las*) vertical recast(connect) order( pre_10 pre_9 pre_8 pre_7 pre_6 pre_5 pre_4 pre_3 pre_2 pre_1 current las_1 las_2 las_3 las_4 las_5 las_6 las_7 las_8 las_9 las_10 ) yline(0,lp(solid) lc(black)) xline(11,lp(solid)) ytitle("政策效应") xtitle("政策时点") xlabel(1 "-10" 2 "-9" 3 "-8" 4 "-7" 5 "-6" 6 "-5" 7 "-4" 8 "-3" 9 "-2" 10 "-1" 11 "0" 12 "1" 13 "2" 14 "3" 15 "4" 16 "5" 17 "6" 18 "7" 19 "8" 20 "9" 21 "10") ciopts(recast(rcap) lc(black) lp(dash) lw(thin)) scale(1.0) name(平行趋势图_graph, replace)
//pre_1 按顺序放
graph export "平行趋势图_graph.jpg", as(jpg) replace
 
 
//安慰剂检验 随机城市和随机政策时间
cd /Users/chengguniang/Documents/论文/智慧城市试点政策对城市韧性的影响/实证
clear 

mat b = J(2000,1,.)
 
mat se = J(2000,1,.)
 
mat p = J(2000,1,.)

forvalues i = 1/2000 {
    use "/Users/chengguniang/Documents/论文/智慧城市试点政策对城市韧性的影响/实证/实证表.dta" 
	xtset city1 year 
	keep if year == 2002
	sample 95 ,count
	keep city1 
	save matchcity.dta , replace
	merge 1:m city1 using 实证表.dta
	gen treat1 = (_merge ==3)
	save matchcity`i'.dta , replace
	use "/Users/chengguniang/Documents/论文/智慧城市试点政策对城市韧性的影响/实证/实证表.dta" 
	bsample 1 , strata(city1)
	keep year 
	save matchyear.dta ,replace 
	mkmat year , matrix(sampleyear)
	use matchcity`i'.dta,replace
	xtset city1 year
	global x "互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w "
	gen time1 = 0 
	foreach j of numlist 1/297{
	replace time1 = 1 if (city1 == `j'& year >= sampleyear[`j',1] )
	}
	gen DID = time1 * treat1
	qui xtreg score_w DID $x i.year ,fe vce(cluster city1)
	mat b[`i',1] = _b[DID]
	mat se[`i',1] = _se[DID]
	scalar df_r = e(N) - e(df_m) -1
	mat p[`i',1] = 2*ttail(df_r,abs(_b[DID]/_se[DID]))
	drop _all
}

svmat b , names(coef)
svmat se, names(se)
svmat p , names(pvalue)

drop if pvalue1 ==.
label var pvalue1 p值
label var coef1 估计系数

twoway (scatter pvalue1 coef1, xlabel(-0.006(0.002)0.006, grid) xtitle("估计系数") ytitle("p值") yline(0.1, lp(shortdash)) xline(0.00368, lp(shortdash))  msymbol(smcircle_hollow) mcolor(blue)msize(small) yaxis(1) ylabel(0(2)6)) (kdensity coef1, lcolor(black) lwidth(medium) ytitle("密度", axis(2))  yaxis(2)),legend(off) name(combined_graph, replace)

graph export "combined_graph.jpg", as(jpg) replace

forvalue i=1/2000{
    erase matchcity`i'.dta
}


//倾向匹配得分匹配
clear
use "/Users/knight/Desktop/智慧城市与城市韧性/重新实证/实证表.dta" 
forvalue i = 2002/2022{
      preserve
          capture {
              keep if year == `i'
              set seed 0000
              gen  norvar_2 = rnormal()
              sort norvar_2

              psmatch2 treat  互联网占比_w  第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w ,outcome(score_w) logit neighbor(1) 
              save `i'.dta, replace
              }
      restore
      }

clear all

use 2002.dta, clear

forvalue k =2002/2022 {
      capture {
          append using `k'.dta
          }
      }

save ybydata.dta, replace

**# 2.2 倾向得分值的核密度图

sum _pscore if treat == 1, detail  // 处理组的倾向得分均值为0.5698 0.336541

/*
                 psmatch2: Propensity Score
-------------------------------------------------------------
      Percentiles      Smallest
 1%     .2462649       .0970248
 5%     .3280515       .1070863
10%     .3792415       .1209767       Obs              43,199
25%     .4720953       .1223726       Sum of Wgt.      43,199

50%     .5749079                      Mean            .569827
                        Largest       Std. Dev.      .1409699
75%     .6723992       .9418448
90%     .7531485       .9431685       Variance       .0198725
95%     .7943281       .9442054       Skewness      -.1636505
99%     .8578832       .9488408       Kurtosis       2.565248
*/

*- 匹配前

sum _pscore if treat == 0, detail

/*
                 psmatch2: Propensity Score
-------------------------------------------------------------
      Percentiles      Smallest
 1%      .202491       .0956568
 5%     .2692991       .0965333
10%     .3099461       .1020349       Obs              38,368
25%     .3875343       .1085678       Sum of Wgt.      38,368

50%     .4792712                      Mean           .4843371
                        Largest       Std. Dev.      .1361458
75%     .5741765       .9391565
90%     .6637404       .9421781       Variance       .0185357
95%     .7196828       .9497313       Skewness       .2292698
99%     .8224753       .9510892       Kurtosis       2.825848
*/

twoway(kdensity _pscore if treat == 1, lpattern(solid)                     ///
              lcolor(black)                                                  ///
              lwidth(thin)                                                   ///
              scheme(qleanmono)                                              ///
              ytitle("{stSans:核}""{stSans:密}""{stSans:度}",                ///
                     size(medlarge) orientation(h))                          ///
              xtitle("{stSans:匹配前的倾向得分值}",                          ///
                     size(medlarge))                                         ///
              xline(0.313   , lpattern(solid) lcolor(black))                ///
              xline(`r(mean)', lpattern(dash)  lcolor(black))  xscale(range(0 0.6))         ///
              saving(kensity_yby_before, replace))                           ///
      (kdensity _pscore if treat == 0, lpattern(dash)),                    ///
      xlabel( 0(0.1)0.6    , labsize(medlarge)  format(%02.1f))                        ///
      ylabel(0(2)10, labsize(medlarge))                                      ///
      legend(label(1 "{stSans:处理组}")                                      ///
             label(2 "{stSans:控制组}")                                      ///
             size(medlarge) position(1) symxsize(10))

graph export "kensity_yby_before.jpg", replace

discard

*- 匹配后

sum _pscore if treat == 0 & _weight != ., detail

/*
                 psmatch2: Propensity Score
-------------------------------------------------------------
      Percentiles      Smallest
 1%      .218586       .1087206
 5%     .2908361        .116991
10%     .3341125       .1171478       Obs              29,017
25%     .4137743       .1252564       Sum of Wgt.      29,017

50%     .5051881                      Mean           .5081499
                        Largest       Std. Dev.      .1347087
75%     .5981689       .9180007
90%      .684632       .9205208       Variance       .0181464
95%     .7385517       .9307409       Skewness       .1364443
99%     .8327231       .9319533       Kurtosis       2.767411
*/

twoway(kdensity _pscore if treat == 1, lpattern(solid)                     ///
              lcolor(black)                                                  ///
              lwidth(thin)                                                   ///
              scheme(qleanmono)                                              ///
              ytitle("{stSans:核}""{stSans:密}""{stSans:度}",                ///
                     size(medlarge) orientation(h))                          ///
              xtitle("{stSans:匹配后的倾向得分值}",                          ///
                     size(medlarge))                                         ///
              xline(0.329   , lpattern(solid) lcolor(black))                ///
              xline(`r(mean)', lpattern(dash)  lcolor(black))   xscale(range(0 0.6))   ///
              saving(kensity_yby_after, replace))                            ///
      (kdensity _pscore if treat == 0 & _weight != ., lpattern(dash)),     ///
      xlabel(  0(0.1)0.6   , labsize(medlarge)  format(%02.1f))                        ///
      ylabel(0(2)10, labsize(medlarge))                      ///
      legend(label(1 "{stSans:处理组}")                                      ///
             label(2 "{stSans:控制组}")                                      ///
             size(medlarge) position(1) symxsize(10))

graph export "kensity_yby_after.jpg", replace

discard



**# 2.3 逐年平衡性检验

*- 匹配前

forvalue i = 1998/2007 {
          capture {
              qui: logit treated $xlist i.ind3 if year == `i', vce(cluster prov)
              est store ybyb`i'
              estadd local Industry "Yes"
              }
          }

local ybyblist ybyb1998 ybyb1999 ybyb2000 ybyb2001 ybyb2002                  ///
               ybyb2003 ybyb2004 ybyb2005 ybyb2006 ybyb2007

reg2docx `ybyblist' using 逐年平衡性检验结果_匹配前.docx, b(%6.4f) t(%6.4f)  ///
         scalars(N r2_p(%6.4f)) noconstant replace                           ///
         indicate("Industry = *.ind3")                                       ///
         mtitles("1998b" "1999b" "2000b" "2001b" "2002b"                     ///
                 "2003b" "2004b" "2005b" "2006b" "2007b")                    ///
         title("逐年平衡性检验_匹配前")

		 
		 
		 
*- 匹配后

forvalue i = 1998/2007 {
          capture {
              qui: logit treated $xlist i.ind3                               ///
                       if year == `i' & _weight != ., vce(cluster prov)
              est store ybya`i'
              estadd local Industry "Yes"
              }
          }

local ybyalist ybya1998 ybya1999 ybya2000 ybya2001 ybya2002                  ///
               ybya2003 ybya2004 ybya2005 ybya2006 ybya2007

reg2docx `ybyalist' using 逐年平衡性检验结果_匹配后.docx, b(%6.4f) t(%6.4f)  ///
         scalars(N r2_p(%6.4f)) noconstant replace                           ///
         indicate("Industry = *.ind3")                                       ///
         mtitles("1998a" "1999a" "2000a" "2001a" "2002a"                     ///
                 "2003a" "2004a" "2005a" "2006a" "2007a")                    ///
         title("逐年平衡性检验_匹配后")
		 
		 
		 
		 
		 
		 
		 
		 


//匹配后回归
clear
use ybydata.dta
gen     weight = _weight * 2
replace weight = 1 if treat == 1 & _weight != .
gen weight1 = round(weight)
//使用频数加权回归
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w [weight = weight1], absorb(id year)  vce(cluster id)


//内生性检验
//两个 工具变量回归
 use "/Users/chengguniang/Documents/论文/智慧城市试点政策对城市韧性的影响/实证/实证表.dta"
xtset id year

gen 创业活跃度1 = 城镇私营和个体从业人员数人1 /年末单位从业人员数万人_1_2
gen 地形起伏度交互项= 地形起伏度*year

ivreg2 score_w  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w  i.year (智慧城市试点  = 地形起伏度交互项 创业活跃度1), cluster(id) first savefp(first)
est store second
esttab first智慧城市试点 second using did内生性检验1.rtf, replace b(%12.3f) se(%12.3f) nogap compress s(N r2 r2_a)star(* 0.1 ** 0.05 *** 0.01)
//检验通过，不存在内生性问题

//稳健性检验1 
//非0权重回归
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if _weight != 0 , absorb(id year)  vce(cluster id)
//（使用满足共同支撑假设的样本）

use "/Users/chengguniang/Documents/论文/智慧城市试点政策对城市韧性的影响/实证/ybydata.dta"

xtset id year
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if _support==1 , absorb(id year)  vce(cluster id)
est store fe2 //稳健性检验1 匹配后回归
esttab fe2 using did稳健性结果1.rtf, replace b(%12.3f) se(%12.3f) nogap compress s(N r2 r2_a)star(* 0.1 ** 0.05 *** 0.01)

//稳健性检验2 加控制变量
reghdfe score_w 智慧城市试点  互联网占比_w   第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w 教育支出占比  , absorb(id year)  vce(cluster id)
est store a1
//稳健性检验3 改变年份 
reghdfe score_w 智慧城市试点  互联网占比_w   第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w  if year< 2020 , absorb(id year)  vce(cluster id)
est store a2
//稳健性检验4 改变聚类层次
reghdfe score_w 智慧城市试点  互联网占比_w   第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w  , absorb(id year)  vce(cluster year)
est store a3
reghdfe score_w 智慧城市试点  互联网占比_w   第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w  , absorb(id year)  vce(robust)
est store a4
esttab a1 a2 a3 a4 using did稳健性结果2.rtf, replace b(%12.3f) se(%12.3f) nogap compress s(N r2 r2_a)star(* 0.1 ** 0.05 *** 0.01)
//机制检验
// 科技支出占比 一般公共预算支出 普通高等学校专任教师数人_0_0 普通高等学校学校数所_0_0

xtset id year
reghdfe 科技支出占比 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w  , absorb(id year)  vce(cluster id)
est store b1
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w 科技支出占比 , absorb(id year)  vce(cluster id)
est store b2
reghdfe 普通高等学校学校数所_0_0 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w  , absorb(id year)  vce(cluster id)
est store b3
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w 普通高等学校学校数所_0_0 , absorb(id year)  vce(cluster id)
est store b4
esttab b1 b2 b3 b4 using 机制检验结果.rtf, replace b(%12.3f) se(%12.3f) nogap compress s(N r2 r2_a)star(* 0.1 ** 0.05 *** 0.01)






//异质性分析
// egen y1 = median(生态) 生成中位数

//中东西
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if 东中西 == 1, absorb(id year)  vce(cluster id)
est store c1
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if 东中西 == 2, absorb(id year)  vce(cluster id)
est store c2
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if 东中西 == 3, absorb(id year)  vce(cluster id)
est store c3 
//高低生态
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if mean生态> y1, absorb(id year)  vce(cluster id)
est store c4 
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if mean生态< y1, absorb(id year)  vce(cluster id)
est store c5
//高低经济
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if mean经济> y2, absorb(id year)  vce(cluster id)
est store c6
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if mean经济< y2, absorb(id year)  vce(cluster id)
est store c7
//高低社会
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if mean社会> y3, absorb(id year)  vce(cluster id)
est store c8
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if mean社会< y3, absorb(id year)  vce(cluster id)
est store c9
//高低工程
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if mean工程> y4, absorb(id year)  vce(cluster id)
est store c10
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if mean工程< y4, absorb(id year)  vce(cluster id)
est store c11

esttab c* using 异质性分析结果.rtf, replace b(%12.3f) se(%12.3f) nogap compress s(N r2 r2_a)star(* 0.1 ** 0.05 *** 0.01)




//中东西
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if 东中西 == 1, absorb(id year)  vce(robust)
est store c1
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if 东中西 == 2, absorb(id year)  vce(robust)
est store c2
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if 东中西 == 3, absorb(id year)  vce(robust)
est store c3 
//高低生态
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if mean生态> y1, absorb(id year)  vce(robust)
est store c4 
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if mean生态< y1, absorb(id year)  vce(robust)
est store c5
//高低经济
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if mean经济> y2, absorb(id year)  vce(robust)
est store c6
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if mean经济< y2, absorb(id year)  vce(robust)
est store c7
//高低社会
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if mean社会> y3, absorb(id year)  vce(robust)
est store c8
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if mean社会< y3, absorb(id year)  vce(robust)
est store c9
//高低工程
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if mean工程> y4, absorb(id year)  vce(robust)
est store c10
reghdfe score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if mean工程< y4, absorb(id year)  vce(robust)
est store c11

esttab c* using 异质性分析结果1.rtf, replace b(%12.3f) se(%12.3f) nogap compress s(N r2 r2_a)star(* 0.1 ** 0.05 *** 0.01)



//空间DID

**二、莫兰散点图
use 示例数据2.dta,clear


xtset id year

**（1）计算全局莫兰和局部莫兰指数

spatwmat using C:\Users\huawei\Desktop\面板数据空间计量\数据\假设已有矩阵\WDD.dta, name(W) standardize    //调用矩阵，并进行标准化！


spatwmat using WDD.dta, name(W) standardize    //调用矩阵，并进行标准化！





xtmoran y, wname(C:\Users\huawei\Desktop\面板数据空间计量\数据\假设已有矩阵\WDD.dta)  morani(2011 2018) graph symbol(省)   //计算全局莫兰和局部莫兰指数

graph combine picture2011 picture2018, altshrink




asdoc xtmoran y, wname(C:\Users\huawei\Desktop\面板数据空间计量\数据\假设已有矩阵\WDD.dta)  morani(2011 2018) graph symbol(省)  replace  dec(4)   //结果导出

//append选项代表追加      dec(4) 代表保留4位小数


*地名太长，保留两个字符

cap drop pro
generate pro = ustrregexs(0) if ustrregexm(省, "^(..)")    //^是正则表达式中的开始符号，(..)是一个捕获组，用于捕获匹配的字符。


xtmoran y, wname(C:\Users\huawei\Desktop\面板数据空间计量\数据\假设已有矩阵\WDD.dta) morani(2011 2018) graph symbol(pro)   //计算全局莫兰和局部莫兰指数

graph combine picture2011 picture2018, altshrink




**5 LR检验 sdm sar sem
xsmle score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w  , fe model(sdm) wmat(W) type(both) nolog 
est store sdm
xsmle score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w , fe model(sar) wmat(W) type(both) nolog noeffects
est store sar
xsmle score_w 智慧城市试点  互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w , fe model(sem) emat(W) type(both) nolog noeffects
est store sem


lrtest sdm sar ,df(8)    //H0：空间杜宾模型可以简化为空间滞后模型（SAR）
lrtest sdm sem ,df(8)     //H0：空间杜宾模型可以简化为空间误差模型（SEM）


spatwmat using w1.dta, name(W) standardize
xtmoran score_w ,  wname(w1.dta) morani(2002 2022) graph symbol(地区) 


xtmoran score_w ,  wname(Users/knight/Desktop/智慧城市与城市韧性/重新实证/w1.dta) morani(2002 2022) graph symbol(地区) 



////

reghdfe score_w mean空气质量 , absorb(cityid year)
est store a11
reghdfe score_w mean空气质量 人均固定资产_1_2 互联网占比_w 第三产业比第二产业_1_2_w 就业率_w , absorb(cityid year)
est store a12

est store a12
esttab a12 using 数字结果.rtf, replace b(%12.3f) se(%12.3f) nogap compress s(N r2 r2_a)star(* 0.1 ** 0.05 *** 0.01)







gen did1 = 智慧城市试点
replace did1 = 1 if pre_1 == 1
gen did2 = did1 
replace did2 = 1 if pre_2 == 1
gen did3 = did2 
replace did3 = 1 if pre_3 ==1


//转单期

reghdfe score_w  did1 互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w , absorb(id year)  vce(cluster id)

est store b11

reghdfe score_w  智慧城市试点_w 互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if year > 2006 & year < 2018 , absorb(id year)  vce(cluster id)
est store b12

esttab b11 b12 using 新稳健结果.rtf, replace b(%12.3f) se(%12.3f) nogap compress s(N r2 r2_a)star(* 0.1 ** 0.05 *** 0.01)



/////采用Callaway和Sant’Anna（2021）的回归、逆概率加权（inverse-probability-weighted）或双重稳健估计（doubly-robust）变体。这些方法可以处理不同的干预时机和潜在的偏差，提供更为准确的估计结果。

gen treattime = action
replace treattime = 0 if action == .
csdid score_w 互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w if year > 2006 & year < 2019 , ivar(id ) time(year) gvar(treattime )


gen treattime = 0
replace treattime = 2011 if treat == 1
csdid score_w 互联网占比_w    第三产业比第二产业_1_2_w 就业率_w 固定资产投资占比_w  if year > 2005 & year < 2017 , ivar(id) time(year) gvar(treattime )  cluster(省份id)









