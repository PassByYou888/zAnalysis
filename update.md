**2018-6-22**

**更新列表**
- 修复JLS无损压缩的格式不兼容JPEG问题（现在每次打开JLG会检查3个标签，不会再出现不兼容错误）
- 将JLS无损压缩的Log提示改用了DoStatus
- JLS无损压缩已经通过官方JLS样本测试
- 将内存光栅库的字节序格式改为了标准BGRA格式
- 修复内存光栅库中色彩到浮点转换bug
- sift金字塔系统不再是只支持灰度，现在支持尺度色差
- zDrawEngine中的方框绘制，方框填充，文字绘制，现在支持旋转特性
- 多线程渲染的数据交换需求：zDrawEngine中的Command List，现在可以被复制出来
- 修改了DoStatus的锁机制，现在DoStatus支持在并行线程打印，不再会发生线程锁死，调试更方便
- 大量更新Gemetry2DUnit中的函数命名
- 移除一个在CoreAtomic.inc中的检查函数，小幅提升多线程性能
- 将TLearn的Constructor修饰符以Class function替代

**新增功能**

- 新增纯pascal实现的H264软编码器，全平台已测试通过
- 新增YUV4MPEG格式支持，全平台已测试通过
- 新增高保真AGG库，剔除了原AGG库中的所有字体系统，大面积修复AGG的编译和跨平台问题，，全平台已测试通过
- 新增基于三角插值模拟矩阵运算的软件渲染器
- 软件渲染器已与H264+YUV4MPEG相互贯通，可以在渲染中同时输出视频
- 新增反走样字体系统，反走样字体系统跨平台，良好支持FPC+Delphi
- 新增高速梯度直方图的特征算法
- 在MemoryRaster库内置了AGG系统，反走样字体系统，三角填充系统
- 基于zDrawEngine新增了直接向MemoryRaster输出的软件渲染接口
- 在Learn新增了梯度直方图的全面性支持
- 在Learn新增了Sift的部分支持
- 新增FontBuild工具(创建跨平台字体库)
- 新增Demo：15 yuv视频转换h264
- 新增Demo：16 软渲染器
- 新增Demo：18 基于梯度方向的大规模图片特征码提取与识别
- 新增Demo：19 基于神经元的梯度方向识别


**2018-5-24**
- 根据客户要求，新增一个区域内容识别的Demo，内置了部分与DrawEngine相关的绘制编程范式，使用方法等等
- 根据客户要求，识别算法精度有略微提高
- 新增一份使用DrawEngine在IOS和Android真机实现硬件加速的技术文档

**2018-5-21**
- 彻底移除外部并行化支持库 pasmp.pas
- Learn.pas内核新增并行线程机制
- 新增对HPC后台服务器支持
- Learn.pas内核新增大量矩阵和向量操作及采样函数
- Geometry2DUnit.pas内核大量更新
- 新增Jpeg无损压缩（平台无关性），并且和Jpeg的小字节序D8FF区分，简单来说和Jpeg无任何冲突，对RGBA的压缩率远比Png要好，无损压缩算法分别支持3种模式：灰度,RGB,RGBA
- 在Tools中BitmapConver工具现在支持无损压缩JpegLS的转换
- 新增一套小型渲染引擎系统：DrawEngine.pas(支持并行化渲染，多线程渲染，平台无关性支持fpc和delphi) + DrawEngineInterface_SlowFMX.pas(需求FMX平台，不支持fpc)
- 新增一个弹道引擎库：BulletMovementEngine.pas(平台无关性)
- 新增zSound库：zSound.pas(平台无关性) + bass(平台无关性) + fmxsound(只支持fmx平台)
- 新增多媒体中心：MediaCenter.pas(平台无关性) 
- 暂时没有做对ffmpeg视频流的支持：**已经过另一个ffmpeg作者同意，zAnalysis结合ffmpeg近期开源版本将支持商业VR功能，感谢ffmpeg作者支持开源，请大家有空多多关照该作者，源作者叫陈省，10年ffmpeg鼓捣经历 http://www.flashavconverter.com/**
- 新增sift高斯金字塔，sift完全支持并行化，21个可调整参量，可以覆盖几乎所有的图片特征向量的发掘和处理
- 新增一个sift算法核的Demo（只演示了算法核，无场景模型支持，如需应用于生产，请联系我做优化和集成工作）
- **注意：sift高斯金字塔不支持分色尺度和分色差分计算，内核是把彩色转换成灰度后进行的尺度和差分计算**
- **注意：sift高斯金字塔不支持单应性矩阵**
- **注意：sift高斯金字塔不支持模式识别，需要手动判断场景模式**
- **注意：对500亿以上的平方运算仍然使用的传统浮点+并行化，未使用sse2,avx512,gpu等新技术接口，图片过于庞大时将会非常耗时**
- **注意：本次更新未编写任何使用文档，具体使用zAnalysis请参考Demo范式以及阅读开源代码**

**2018-4-22**
- 新增5组图片效果化处理：反锯齿，色调平均化，移除照片红眼，Sepia色彩系，噪点移除

**2018-4-18**
- 新增中文GBK字符集支持(内置小字典，支持拼音音标解码，拼音排序，支持GBK字符集修复，繁体，港繁体，台湾繁体等互相转码)
- 新增替代TStringList的字符串链表类，TListPascalString可以支持上亿条Unicode存储
- 新增Hash查询类：THashStringList（支持千万条目级查询）
- 支持.CSV格式导入

**2018-4-15**
- 修复KDTree因为编程疏忽出现的跳级的问题
- 修复所有跟图像识别有关Demo的采样问题
- 修复字符串输出过长，不太容易阅读输出值的问题
- 新增一个控制台Demo：**基于神经网络聚类之决策树**

