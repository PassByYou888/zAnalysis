# zAnalysis是什么？ #

zAnalysis是基于Pascal语言编写开源机器学习库，它不依赖于任何三方库，并且可以跨平台和并行化计算

zAnalysis不限制你的任何商业用途和拷贝，包括二次开发

# 现在zAnalysis还处于开发中状态 补完计划 #
- 完善使用文档和Demo
- 金融领域数据统计和分析的傻瓜支持
- 中小型企业级数据统计和分析的傻瓜支持
- 在游戏领域使用机器学习
- 自然语言处理的傻瓜支持
- 加强视觉库的傻瓜支持(物体识别支持，人脸识别支持，SIFT基础支持库)
- 对第三方大数据源支持(例如google提供的大数据源)
- 可二次开发的模型化机器学习平台
- 分布式云服务器计算后台
- 提供plot二维可视化图形api


# 运行平台支持 #
- **IOS armv7 100%兼容**
- **IOS arm64 100%兼容**
- **Anroid 100%兼容**
- **Windows x86/100%兼容**
- **Windows x64/100%兼容**
- **Linux x64/100%兼容**
- **OSX x86/100%兼容**
 

> 不兼容
- Linux x32/不兼容
- OSX x64/不兼容


# 开发平台支持 #
- Freepascal with Lazarus
- Delphi XE10 或以上版本



# zAnalysis已完成了哪些支持功能 #


**神经网络支持**
- 动态KD空间树支持(纯数组的非链表计算内核，支持并行化查找，内置临近K聚类，只支持双浮点，内置测试框架)
- 高速静态KD空间树支持(纯数组的非链表计算内核，支持并行化查找，内置临近K聚类，支持8种整型和浮点变量类型，内置测试框架)
- K临近聚类支持(结合KDTree)
- LBFGS-大规模拟牛顿法学习(支持并行化，支持神经网络集成) 
- Levenberg-Marquardt-高斯牛顿法学习(支持并行化，支持神经网络集成) 
- 随机森林决策树学习
- 评定模型的逻辑回归
- 蒙特卡罗抽样学习

**正太分布检测**
- F检测
- 二项式化分布检测
- 方差分布检测
- t分布检测
- 皮尔逊分布检测
- 斯皮尔曼分布检测

**正太分布计算**
- 正太分布函数
- 泊松分布
- F分布
- 二项式化分布支持计算
- 方差分布计算
- t分布计算
- 皮尔逊分布
- 斯皮尔曼分布
- Jarque-Bera检验计算
- 曼-惠特尼U检验计算
- 威尔科克森符号秩检验计算

**基础视觉库**
- 图片相似性相关：基于方差图采样(分别支持PCA主成分分析与LDA线性判别分析)
- 图片相似性相关：基于皮尔逊,斯皮尔曼,k空间的相似性查找
- 最小走样化缩放(专业支持的最小走样缩放，非常规处理方法)
- 光栅图片库支持FMX内置集成

**数据库集成**
- 内置ZDB本地数据库，ZDB可以作为大型数据库的伴侣数据库，为项目提供统计学支持
- 内置ZDB内存数据库支持
- ZDB可以轻松改造成网络数据库后台，并且能基于zServer项目轻松搭建机器学习的后台框架

**其它支持**
- 提供快速存储与恢复，可以动态读取，计算结果，无需训练
- 部分数据格式可以兼容MATLAB
- 机器学习状态可以导出成Json格式
- 以匿名函数方式训练(匿名函数只能工作于Delphi开发平台，不支持fpc)
- 安全浮点运算
- 安全动态数组处理
- 零内存泄漏
- 工具集：MemoryRaster个图片格式转换工具
- 工具集：文本转换成pascal代码格式工具


# 随机森林决策树的简单演示

```delphi

procedure forest;
	// 这里是我们模拟在应用时的条件处理程序
	procedure runF(id: TLearnFloat);
	begin
	  case Round(id) of
	    1: dostatus('%f hey', [id]);
	    2: dostatus('%f im robot', [id]);
	    3: dostatus('%f nice to meet you', [id]);
	    4: dostatus('%f haha', [id]);
	    5: dostatus('%f lol', [id]);
	    6: dostatus('%f hello world', [id]);
	    7: dostatus('%f byebye', [id]);
	    else raiseInfo('%f no memory', [id]);
	  end;
	end;
var
  lr: TLearn;
  i : Integer;
begin
  // 随机森林决策模型
  // 随机森林决策模型在工作时需要逻辑决策回归，OutIn可以1或者更高数值，OutLen只能是1
  lr := TLearn.Create(TLearnType.ltForest, 2, 1);
  lr.AddMemory('0,0 = 1');
  lr.AddMemory('1,1 = 2');
  lr.AddMemory('1,0 = 3');
  lr.AddMemory('0,1 = 4');
  lr.AddMemory('4,5 = 5');
  lr.AddMemory('3,5 = 6');
  lr.AddMemory('5,3 = 7');

  lr.Train;

  // 这里是我们已经学习过的内容，我们打印出来验证
  runF(lr.processRF([0, 0]));
  runF(lr.processRF([1, 1]));
  runF(lr.processRF([1, 0]));
  runF(lr.processRF([0, 1]));
  runF(lr.processRF([4, 5]));
  runF(lr.processRF([3, 5]));
  runF(lr.processRF([5, 3]));

  // 随机值 推理学习
  // 随机森林决策模型会严格的从已经学习到的Out值中去寻找和推理最佳符合条件
  // 随机森林适用于复杂的条件处理程序
  for i := 1 to 10 do
    begin
      runF(lr.processRF([umlRandomRange(-100, 100), umlRandomRange(-100, 100)]));
      TCoreClassThread.Sleep(100);
    end;

  disposeObject(lr);
end;
```


**如果你支持zAnalysis开发，请向作者捐款，希望捐赠后能留下真实姓名和联系方式，开发建议请发至作者邮箱** [600585@qq.com](mailto:600585@qq.com "600585@qq.com")

![](alipay.jpg)



使用问题请加在互助qq群490269542
 
请不要直接加作者，谢谢大家

2018-4

