{ ****************************************************************************** }
{ * machine Learn support, writen by QQ 600585@qq.com                          * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }
unit Learn;

interface

uses CoreClasses, UnicodeMixedLib, PascalStrings, MemoryRaster, KDTree, KM,
  DataFrameEngine, MemoryStream64;

{$I ZDefine.inc}


type
  TLearnFloat        = TKDTree_VecType;
  PLearnFloat        = PKDTree_VecType;
  TLearnFloatArray   = TKDTree_Vec;
  PLearnFloatArray   = PKDTree_Vec;
  TLearnFloat2DArray = TKDTree_DynamicVecBuffer;
  PLearnFloat2DArray = PKDTree_DynamicVecBuffer;

  TLearnVector = TLearnFloatArray;
  PLearnVector = PLearnFloatArray;

  TLearnMatrix = TLearnFloat2DArray;
  PLearnMatrix = PLearnFloat2DArray;

  TLearnInteger = Integer;
  PLearnInteger = ^TLearnInteger;

  TLearnIntegerArray   = packed array of TLearnInteger;
  PLearnIntegerArray   = ^TLearnIntegerArray;
  TLearnInteger2DArray = packed array of TLearnIntegerArray;

  TLearnType = (
    ltKDT,              // KDTree, fast space operation, this not Neurons network
    ltKM,               // k-means++ clusterization, this not Neurons network
    ltForest,           // random decision forest
    ltLogit,            // Logistic regression
    ltLM,               // Levenberg-Marquardt
    ltLM_MT,            // Levenberg-Marquardt with parallel
    ltLBFGS,            // L-BFGS
    ltLBFGS_MT,         // L-BFGS with parallel
    ltLBFGS_MT_Mod,     // L-BFGS with parallel and optimization
    ltMonteCarlo,       // fast Monte Carlo train
    ltLM_Ensemble,      // Levenberg-Marquardt Ensemble
    ltLM_Ensemble_MT,   // Levenberg-Marquardt Ensemble with parallel
    ltLBFGS_Ensemble,   // L-BFGS Ensemble
    ltLBFGS_Ensemble_MT // L-BFGS Ensemble with parallel
    );

  TLearn = class;

  TLearnState_Call               = procedure(const LSender: TLearn; const state: Boolean);
  TLearnState_Method             = procedure(const LSender: TLearn; const state: Boolean) of object;
  {$IFNDEF FPC} TLearnState_Proc = reference to procedure(const LSender: TLearn; const state: Boolean); {$ENDIF}
  PLearnMemory                   = ^TLearnMemory;

  TLearnMemory = packed record
    m_in, m_out: TLearnFloatArray;
  end;

  TLearn = class(TCoreClassInterfacedObject)
  private
    FEnabledRandomNumber                       : Boolean;
    FInLen, FOutLen                            : TLearnInteger;
    FMemorySource                              : TCoreClassList;
    FLearnType                                 : TLearnType;
    FLearnData                                 : Pointer;
    FLastTrainMaxInValue, FLastTrainMaxOutValue: TLearnFloat;
    FInfo                                      : string;
    FIsTraining                                : Boolean;
    FTrainThreadRuning                         : Boolean;

    procedure KDInput(const IndexFor: NativeInt; var source: TKDTree_Source; const Data: Pointer);
  public
    constructor Create(const lt: TLearnType; const AInLen, AOutLen: TLearnInteger); overload;
    constructor Create(const AInLen, AOutLen: TLearnInteger); overload;
    destructor Destroy; override;

    { * fixed random number * }
    property EnabledRandomNumber: Boolean read FEnabledRandomNumber write FEnabledRandomNumber;

    { * clear * }
    procedure Clear;

    { * parameter support * }
    function Count: TLearnInteger;
    property InLen: TLearnInteger read FInLen;
    property OutLen: TLearnInteger read FOutLen;
    property Info: string read FInfo;
    property TrainThreadRuning: Boolean read FTrainThreadRuning;
    function GetMemorySource(const index: TLearnInteger): PLearnMemory;
    property MemorySource[const index: TLearnInteger]: PLearnMemory read GetMemorySource; default;

    { * dynamic memory support * }
    procedure AddMemory(const f_In, f_Out: TLearnFloatArray); overload;
    procedure AddMemory(const s_In, s_Out: string); overload;
    procedure AddMemory(const s: TPascalString); overload;
    { * PCA memory sampler * }
    procedure AddMemorySamplerWithPCA(const Fast: Boolean; const mr_in: TMemoryRaster; const f_Out: TLearnFloatArray); overload;
    { * LDA memory sampler * }
    procedure AddMemorySamplerWithLDA(const Fast: Boolean; const mr_in: TMemoryRaster; const f_Out: TLearnFloatArray); overload;

    { * normal train * }
    function Train(const TrainDepth: TLearnInteger): Boolean; overload;
    function Train: Boolean; overload;
    { * train with thread support * }
    procedure Train_MT; overload;
    procedure Train_MT(const TrainDepth: TLearnInteger); overload;
    procedure TrainC(const TrainDepth: TLearnInteger; const OnResult: TLearnState_Call);
    procedure TrainM(const TrainDepth: TLearnInteger; const OnResult: TLearnState_Method);
    {$IFNDEF FPC} procedure TrainP(const TrainDepth: TLearnInteger; const OnResult: TLearnState_Proc); {$ENDIF FPC}
    procedure WaitTrain;
    //
    // result ProcessOut
    function process(const ProcessIn, ProcessOut: PLearnFloatArray): Boolean; overload;
    function process(const ProcessIn: PLearnFloatArray): string; overload;
    function process(const ProcessIn: TLearnFloatArray): string; overload;
    function process(const ProcessIn: TPascalString): string; overload;
    // result max value
    function processMax(const ProcessIn: TLearnFloatArray): TLearnFloat; overload;
    // result max index
    function processMaxIndex(const ProcessIn: TLearnFloatArray): TLearnInteger; overload;
    // result min value
    function processMin(const ProcessIn: TLearnFloatArray): TLearnFloat; overload;
    // result min index
    function processMinIndex(const ProcessIn: TLearnFloatArray): TLearnInteger; overload;
    // result first value
    function processRF(const ProcessIn: TLearnFloatArray): TLearnFloat; overload;
    function processRF(const ProcessIn: TPascalString): TLearnFloat; overload;
    // result last value
    function processLF(const ProcessIn: TLearnFloatArray): TLearnFloat; overload;
    function processLF(const ProcessIn: TPascalString): TLearnFloat; overload;

    // search with Pearson
    function SearchMemoryWithPearson(const ProcessIn: TLearnFloatArray): TLearnInteger; overload;
    // search with Pearson - parallel support
    procedure SearchMemoryWithPearson(const ProcessIn: TLearnFloatArray; out List: TLearnIntegerArray); overload;

    // search with Spearman
    function SearchMemoryWithSpearman(const ProcessIn: TLearnFloatArray): TLearnInteger; overload;
    // search with Spearman - parallel support
    procedure SearchMemoryWithSpearman(const ProcessIn: TLearnFloatArray; out List: TLearnIntegerArray); overload;

    // search with KDTree
    function SearchMemoryWithKDTree(const ProcessIn: TLearnFloatArray): TLearnInteger; overload;
    // search with KDTree - parallel support
    procedure SearchMemoryWithKDTree(const ProcessIn: TLearnFloatArray; out List: TLearnIntegerArray); overload;

    { * fast binary store support * }
    procedure SaveToDF(df: TDataFrameEngine);
    procedure LoadFromDF(df: TDataFrameEngine);

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    {$IFNDEF FPC}
    { * json store support * }
    procedure SaveToJsonStream(stream: TCoreClassStream);
    procedure LoadFromJsonStream(stream: TCoreClassStream);
    {$ENDIF FPC}
    { * linear discriminant analysis support * }
    function LDA(const buff: TLearnMatrix; const NPoints, NVars, NClasses: TLearnInteger; var w: TLearnMatrix): TLearnInteger; overload;
    function LDA(const buff: TLearnMatrix; const NPoints, NVars, NClasses: TLearnInteger; var w: TLearnVector): TLearnInteger; overload;
    function LDA(const Fast: Boolean; const mr: TMemoryRaster; var output: TLearnMatrix): Boolean; overload;
    function LDA(const Fast: Boolean; const mr: TMemoryRaster; var output: TLearnVector): Boolean; overload;
    function LDA(const Fast: Boolean; const mr1, mr2: TMemoryRaster): TLearnFloat; overload;
    { * principal component analysis support * }
    function PCA(const buff: TLearnMatrix; const NPoints, NVars: TLearnInteger; var v: TLearnMatrix): TLearnInteger; overload;
    function PCA(const Fast: Boolean; const mr: TMemoryRaster; var output: TLearnMatrix): Boolean; overload;
    function PCA(const Fast: Boolean; const mr: TMemoryRaster; var output: TLearnVector): Boolean; overload;
    function PCA(const Fast: Boolean; const mr1, mr2: TMemoryRaster): TLearnFloat; overload;
    { * k-means++ clusterization support * }
    function KMeans(const source: TKMFloat2DArray; const NVars, k: TLearnInteger; var KArray: TKMFloat2DArray; var kIndex: TKMIntegerArray): Boolean;
  end;

  { base type api }
procedure Clamp(var AValue: TLearnFloat; const AMin, AMax: TLearnFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}

function AbsReal(X: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AbsInt(I: TLearnInteger): TLearnInteger; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RandomReal(): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RandomInteger(I: TLearnInteger): TLearnInteger; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Sign(X: TLearnFloat): TLearnInteger; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_Sqr(X: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function APVDotProduct(V1: PLearnFloat; I11, I12: TLearnInteger; V2: PLearnFloat; I21, I22: TLearnInteger): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVMove(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat; I21, I22: TLearnInteger); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVMove(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat; I21, I22: TLearnInteger; s: TLearnFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVMoveNeg(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat; I21, I22: TLearnInteger); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVAdd(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat; I21, I22: TLearnInteger); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVAdd(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat; I21, I22: TLearnInteger; s: TLearnFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVSub(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat; I21, I22: TLearnInteger); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVSub(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat; I21, I22: TLearnInteger; s: TLearnFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVMul(VOp: PLearnFloat; I1, I2: TLearnInteger; s: TLearnFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVFillValue(VOp: PLearnFloat; I1, I2: TLearnInteger; s: TLearnFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}

function AP_Float(X: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_FP_Eq(X: TLearnFloat; Y: TLearnFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_FP_NEq(X: TLearnFloat; Y: TLearnFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_FP_Less(X: TLearnFloat; Y: TLearnFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_FP_Less_Eq(X: TLearnFloat; Y: TLearnFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_FP_Greater(X: TLearnFloat; Y: TLearnFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_FP_Greater_Eq(X: TLearnFloat; Y: TLearnFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

{ matrix support }
function VectorNorm2(const X: TLearnVector; const I1, I2: TLearnInteger): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function VectorIdxAbsMax(const X: TLearnVector; const I1, I2: TLearnInteger): TLearnInteger; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ColumnIdxAbsMax(const X: TLearnMatrix; const I1, I2, J: TLearnInteger): TLearnInteger; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RowIdxAbsMax(const X: TLearnMatrix; const J1, J2, I: TLearnInteger): TLearnInteger; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UpperHessenberg1Norm(const A: TLearnMatrix; const I1, I2, J1, J2: TLearnInteger; var WORK: TLearnVector): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CopyMatrix(const A: TLearnMatrix; const IS1, IS2, JS1, JS2: TLearnInteger;
  var B: TLearnMatrix; const ID1, ID2, JD1, JD2: TLearnInteger); {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure InplaceTranspose(var A: TLearnMatrix; const I1, I2, J1, J2: TLearnInteger; var WORK: TLearnVector); {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CopyAndTranspose(const A: TLearnMatrix; IS1, IS2, JS1, JS2: TLearnInteger;
  var B: TLearnMatrix; ID1, ID2, JD1, JD2: TLearnInteger); {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MatrixVectorMultiply(const A: TLearnMatrix; const I1, I2, J1, J2: TLearnInteger; const Trans: Boolean;
  const X: TLearnVector; const IX1, IX2: TLearnInteger; const Alpha: TLearnFloat;
  var Y: TLearnVector; const IY1, IY2: TLearnInteger; const Beta: TLearnFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}

function Pythag2(X: TLearnFloat; Y: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MatrixMatrixMultiply(const A: TLearnMatrix; const AI1, AI2, AJ1, AJ2: TLearnInteger; const TransA: Boolean;
  const B: TLearnMatrix; const BI1, BI2, BJ1, BJ2: TLearnInteger; const TransB: Boolean;
  const Alpha: TLearnFloat;
  var C: TLearnMatrix; const CI1, CI2, CJ1, CJ2: TLearnInteger;
  const Beta: TLearnFloat;
  var WORK: TLearnVector); {$IFDEF INLINE_ASM} inline; {$ENDIF}

{ Normal distribution support }
function Erf(const X: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ErfC(const X: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function InvErf(const E: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function NormalDistribution(const X: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function InvNormalDistribution(const y0: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}

{ statistics base }
function Log1P(const X: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ExpM1(const X: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CosM1(const X: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Gamma support }
function Gamma(const X: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Natural logarithm of gamma function }
function LnGamma(const X: TLearnFloat; var SgnGam: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Incomplete gamma integral }
function IncompleteGamma(const A, X: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Complemented incomplete gamma integral }
function IncompleteGammaC(const A, X: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Inverse of complemented imcomplete gamma integral }
function InvIncompleteGammaC(const A, y0: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}

{ Poisson distribution }
function PoissonDistribution(k: TLearnInteger; M: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Complemented Poisson distribution }
function PoissonCDistribution(k: TLearnInteger; M: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Inverse Poisson distribution }
function InvPoissonDistribution(k: TLearnInteger; Y: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}

{ Incomplete beta integral support }
function IncompleteBeta(A, B, X: TLearnFloat): TLearnFloat;
{ Inverse of imcomplete beta integral }
function InvIncompleteBeta(const A, B, Y: TLearnFloat): TLearnFloat;

{ F distribution support }
function FDistribution(const A: TLearnInteger; const B: TLearnInteger; const X: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Complemented F distribution }
function FCDistribution(const A: TLearnInteger; const B: TLearnInteger; const X: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Inverse of complemented F distribution }
function InvFDistribution(const A: TLearnInteger; const B: TLearnInteger; const Y: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Two-sample F-test }
procedure FTest(const X: TLearnFloatArray; N: TLearnInteger; const Y: TLearnFloatArray; M: TLearnInteger; var BothTails, LeftTail, RightTail: TLearnFloat);

{ Binomial distribution support }
function BinomialDistribution(const k, N: TLearnInteger; const p: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Complemented binomial distribution }
function BinomialCDistribution(const k, N: TLearnInteger; const p: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Inverse binomial distribution }
function InvBinomialDistribution(const k, N: TLearnInteger; const Y: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Sign test }
procedure OneSampleSignTest(const X: TLearnFloatArray; N: TLearnInteger; Median: TLearnFloat; var BothTails, LeftTail, RightTail: TLearnFloat);

{ Chi-square distribution support }
function ChiSquareDistribution(const v, X: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Complemented Chi-square distribution }
function ChiSquareCDistribution(const v, X: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Inverse of complemented Chi-square distribution }
function InvChiSquareDistribution(const v, Y: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ One-sample chi-square test }
procedure OneSampleVarianceTest(const X: TLearnFloatArray; N: TLearnInteger; Variance: TLearnFloat; var BothTails, LeftTail, RightTail: TLearnFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}

{ Student's t distribution support }
function StudentTDistribution(const k: TLearnInteger; const t: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Functional inverse of Student's t distribution }
function InvStudentTDistribution(const k: TLearnInteger; p: TLearnFloat): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ One-sample t-test }
procedure StudentTTest1(const X: TLearnFloatArray; N: TLearnInteger; Mean: TLearnFloat; var BothTails, LeftTail, RightTail: TLearnFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Two-sample pooled test }
procedure StudentTTest2(const X: TLearnFloatArray; N: TLearnInteger; const Y: TLearnFloatArray; M: TLearnInteger; var BothTails, LeftTail, RightTail: TLearnFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Two-sample unpooled test }
procedure UnequalVarianceTTest(const X: TLearnFloatArray; N: TLearnInteger; const Y: TLearnFloatArray; M: TLearnInteger; var BothTails, LeftTail, RightTail: TLearnFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}

{ Pearson and Spearman distribution support }
{ Pearson product-moment correlation coefficient }
function PearsonCorrelation(const X, Y: TLearnFloatArray; const N: TLearnInteger): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Spearman's rank correlation coefficient }
function SpearmanRankCorrelation(const X, Y: TLearnFloatArray; const N: TLearnInteger): TLearnFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SpearmanRank(var X: TLearnFloatArray; N: TLearnInteger); {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Pearson's correlation coefficient significance test }
procedure PearsonCorrelationSignificance(const R: TLearnFloat; const N: TLearnInteger; var BothTails, LeftTail, RightTail: TLearnFloat);
{ Spearman's rank correlation coefficient significance test }
procedure SpearmanRankCorrelationSignificance(const R: TLearnFloat; const N: TLearnInteger; var BothTails, LeftTail, RightTail: TLearnFloat);

{ Jarque-Bera test }
procedure JarqueBeraTest(const X: TLearnFloatArray; const N: TLearnInteger; var p: TLearnFloat);

{ Mann-Whitney U-test }
procedure MannWhitneyUTest(const X: TLearnFloatArray; N: TLearnInteger; const Y: TLearnFloatArray; M: TLearnInteger; var BothTails, LeftTail, RightTail: TLearnFloat);

{ Wilcoxon signed-rank test }
procedure WilcoxonSignedRankTest(const X: TLearnFloatArray; N: TLearnInteger; E: TLearnFloat; var BothTails, LeftTail, RightTail: TLearnFloat);

const
  MachineEpsilon = 5E-16;
  MaxRealNumber  = 1E300;
  MinRealNumber  = 1E-300;

implementation

uses Math,
  {$IFDEF FPC}
  mtprocs,
  {$ELSE}
  Threading,
  {$ENDIF FPC}
  SyncObjs, DoStatusIO;

procedure Clamp(var AValue: TLearnFloat; const AMin, AMax: TLearnFloat);
begin
  if AMin >= AMax then
      Clamp(AValue, AMax, AMin)
  else
    if AValue > AMax then
      AValue := AMax
  else if AValue < AMin then
      AValue := AMin;
end;

{$INCLUDE Learn_Base.inc}
{$INCLUDE learn_blas.inc}
{$INCLUDE Learn_ablas.inc}
{$INCLUDE learn_trfac.inc}
{$INCLUDE learn_safesolve.inc}
{$INCLUDE learn_rcond.inc}
{$INCLUDE learn_matinv.inc}
{$INCLUDE learn_linmin.inc}
{$INCLUDE learn_lbfgs.inc}
{$INCLUDE learn_rotations.inc}
{$INCLUDE learn_ortfac.inc}
{$INCLUDE learn_bdsvd.inc}
{$INCLUDE learn_svd.inc}
{$INCLUDE learn_densesolver.inc}
{$INCLUDE learn_trainbase.inc}
{$INCLUDE learn_train.inc}
{$INCLUDE learn_trainEnsemble.inc}
{$INCLUDE learn_schur.inc}
{$INCLUDE learn_evd.inc}
{$INCLUDE learn_PCA.inc}
{$INCLUDE learn_LDA.inc}
{$INCLUDE learn_forest.inc}
{$INCLUDE learn_logit.inc}
{$INCLUDE learn_statistics_normal_distribution.inc}
{$INCLUDE learn_statistics_base.inc}
{$INCLUDE learn_statistics_IncompleteBeta.inc}
{$INCLUDE learn_statistics_fdistribution.inc}
{$INCLUDE learn_statistics_binomial_distribution.inc}
{$INCLUDE learn_statistics_chisquare_distribution.inc}
{$INCLUDE learn_statistics_StudentsT_distribution.inc}
{$INCLUDE learn_statistics_Pearson_Spearman.inc}
{$INCLUDE learn_statistics_JarqueBeraTest.inc}
{$INCLUDE learn_statistics_MannWhitneyUTest.inc}
{$INCLUDE learn_statistics_Wilcoxon.inc}


type
  PLearnKDT = ^TLearnKDT;

  TLearnKDT = packed record
    kdt: TKDTree;
  end;

  TLearn_th = class(TCoreClassThread)
  public
    source                : TLearn;
    TrainDepth            : TLearnInteger;
    OnStateC              : TLearnState_Call;
    OnStateM              : TLearnState_Method;
    {$IFNDEF FPC} OnStateP: TLearnState_Proc; {$ENDIF}
    Successed             : Boolean;

    procedure SyncResultState;
    procedure Execute; override;
    constructor Create;
    destructor Destroy; override;
  end;

procedure TLearn_th.SyncResultState;
begin
  if Assigned(OnStateC) then
      OnStateC(source, Successed);
  if Assigned(OnStateM) then
      OnStateM(source, Successed);
  {$IFNDEF FPC}
  if Assigned(OnStateP) then
      OnStateP(source, Successed);
  {$ENDIF FPC}
  source.FTrainThreadRuning := False;
end;

procedure TLearn_th.Execute;
begin
  if source <> nil then
      Successed := source.Train(TrainDepth)
  else
      Successed := False;

  {$IFDEF FPC}
  Synchronize(@SyncResultState);
  {$ELSE FPC}
  Synchronize(SyncResultState);
  {$ENDIF FPC}
end;

constructor TLearn_th.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  source := nil;
  TrainDepth := 2;
  OnStateC := nil;
  OnStateM := nil;
  {$IFNDEF FPC} OnStateP := nil; {$ENDIF}
  Successed := False;
end;

destructor TLearn_th.Destroy;
begin
  inherited Destroy;
end;

procedure TLearn.KDInput(const IndexFor: NativeInt; var source: TKDTree_Source; const Data: Pointer);
var
  I: TLearnInteger;
begin
  source.index := IndexFor;
  for I := 0 to FInLen - 1 do
      source.buff[I] := PLearnMemory(FMemorySource[IndexFor])^.m_in[I];
end;

constructor TLearn.Create(const lt: TLearnType; const AInLen, AOutLen: TLearnInteger);
var
  p_k    : PLearnKDT;
  p_f    : PDecisionForest;
  p_logit: PLogitModel;
  p_n    : PMultiLayerPerceptron;
  p_e    : PMLPEnsemble;
begin
  inherited Create;
  if AInLen <= 0 then
      raiseInfo('input need <= 0');
  if AOutLen <= 0 then
      raiseInfo('output need <= 0');

  FEnabledRandomNumber := False;

  if (AOutLen <> 1) and (lt = ltForest) then
      raiseInfo('ltForest OutLen need = 1');

  if (AOutLen <> 1) and (lt = ltLogit) then
      raiseInfo('ltLogit OutLen need = 1');

  FInLen := AInLen;
  FOutLen := AOutLen;
  FMemorySource := TCoreClassList.Create;
  FLearnType := lt;

  case lt of
    ltKDT, ltKM:
      begin
        new(p_k);
        p_k^.kdt := TKDTree.Create(FInLen);
        FLearnData := p_k;
      end;
    ltForest:
      begin
        new(p_f);
        FLearnData := p_f;
      end;
    ltLogit:
      begin
        new(p_logit);
        FLearnData := p_logit;
      end;
    ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo:
      begin
        new(p_n);
        MLPCreate0(FInLen, FOutLen, p_n^);
        FLearnData := p_n;
      end;
    ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT:
      begin
        new(p_e);
        MLPECreate0(FInLen, FOutLen, 10, p_e^);
        FLearnData := p_e;
      end;
  end;
  FLastTrainMaxInValue := 0;
  FLastTrainMaxOutValue := 0;
  FInfo := '';
  FIsTraining := False;
  FTrainThreadRuning := False;
end;

constructor TLearn.Create(const AInLen, AOutLen: TLearnInteger);
begin
  Create(ltKDT, AInLen, AOutLen);
end;

destructor TLearn.Destroy;
var
  I: TLearnInteger;
begin
  WaitTrain;

  if FMemorySource <> nil then
    begin
      for I := 0 to FMemorySource.Count - 1 do
        begin
          SetLength(PLearnMemory(FMemorySource[I])^.m_in, 0);
          SetLength(PLearnMemory(FMemorySource[I])^.m_out, 0);
          Dispose(PLearnMemory(FMemorySource[I]));
        end;
      DisposeObject(FMemorySource);
      FMemorySource := nil;
    end;

  if FLearnData <> nil then
    begin
      case FLearnType of
        ltKDT, ltKM:
          begin
            DisposeObject(PLearnKDT(FLearnData)^.kdt);
            Dispose(PLearnKDT(FLearnData));
            FLearnData := nil;
          end;
        ltForest:
          begin
            Dispose(PDecisionForest(FLearnData));
            FLearnData := nil;
          end;
        ltLogit:
          begin
            Dispose(PLogitModel(FLearnData));
            FLearnData := nil;
          end;
        ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo:
          begin
            Dispose(PMultiLayerPerceptron(FLearnData));
            FLearnData := nil;
          end;
        ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT:
          begin
            Dispose(PMLPEnsemble(FLearnData));
            FLearnData := nil;
          end;
      end;
    end;

  FInLen := 0;
  FOutLen := 0;
  FInfo := '';
  inherited Destroy;
end;

procedure TLearn.Clear;
var
  I      : TLearnInteger;
  p_k    : PLearnKDT;
  p_f    : PDecisionForest;
  p_logit: PLogitModel;
  p_n    : PMultiLayerPerceptron;
  p_e    : PMLPEnsemble;
begin
  WaitTrain;

  if FMemorySource <> nil then
    begin
      for I := 0 to FMemorySource.Count - 1 do
        begin
          SetLength(PLearnMemory(FMemorySource[I])^.m_in, 0);
          SetLength(PLearnMemory(FMemorySource[I])^.m_out, 0);
          Dispose(PLearnMemory(FMemorySource[I]));
        end;
      DisposeObject(FMemorySource);
      FMemorySource := nil;
    end;

  if FLearnData <> nil then
    begin
      case FLearnType of
        ltKDT, ltKM:
          begin
            DisposeObject(PLearnKDT(FLearnData)^.kdt);
            Dispose(PLearnKDT(FLearnData));
            FLearnData := nil;
          end;
        ltForest:
          begin
            Dispose(PDecisionForest(FLearnData));
            FLearnData := nil;
          end;
        ltLogit:
          begin
            Dispose(PLogitModel(FLearnData));
            FLearnData := nil;
          end;
        ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo:
          begin
            Dispose(PMultiLayerPerceptron(FLearnData));
            FLearnData := nil;
          end;
        ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT:
          begin
            Dispose(PMLPEnsemble(FLearnData));
            FLearnData := nil;
          end;
      end;
    end;

  FMemorySource := TCoreClassList.Create;
  case FLearnType of
    ltKDT, ltKM:
      begin
        new(p_k);
        p_k^.kdt := TKDTree.Create(FInLen);
        FLearnData := p_k;
      end;
    ltForest:
      begin
        new(p_f);
        FLearnData := p_f;
      end;
    ltLogit:
      begin
        new(p_logit);
        FLearnData := p_logit;
      end;
    ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo:
      begin
        new(p_n);
        MLPCreate0(FInLen, FOutLen, p_n^);
        FLearnData := p_n;
      end;
    ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT:
      begin
        new(p_e);
        MLPECreate0(FInLen, FOutLen, 10, p_e^);
        FLearnData := p_e;
      end;
  end;

  FLastTrainMaxInValue := 0;
  FLastTrainMaxOutValue := 0;
  FInfo := '';
end;

function TLearn.Count: TLearnInteger;
begin
  Result := FMemorySource.Count;
end;

function TLearn.GetMemorySource(const index: TLearnInteger): PLearnMemory;
begin
  Result := PLearnMemory(FMemorySource[index]);
end;

procedure TLearn.AddMemory(const f_In, f_Out: TLearnFloatArray);
var
  p: PLearnMemory;
  I: TLearnInteger;
begin
  if FIsTraining or FTrainThreadRuning then
      raiseInfo('wait Training');
  if Length(f_In) <> FInLen then
      raiseInfo('input length need = %d', [FInLen]);
  if Length(f_Out) <> FOutLen then
      raiseInfo('output length need = %d', [FOutLen]);

  new(p);
  SetLength(p^.m_in, FInLen);
  CopyPtr(@f_In[0], @(p^.m_in[0]), FInLen * SizeOf(TLearnFloat));
  SetLength(p^.m_out, FOutLen);
  CopyPtr(@f_Out[0], @(p^.m_out[0]), FOutLen * SizeOf(TLearnFloat));

  FMemorySource.Add(p);
end;

procedure TLearn.AddMemory(const s_In, s_Out: string);
var
  f_In, f_Out: TLearnFloatArray;
begin
  f_In := TKDTree.KDTreeVec(s_In);
  f_Out := TKDTree.KDTreeVec(s_Out);
  AddMemory(f_In, f_Out);
  SetLength(f_In, 0);
  SetLength(f_Out, 0);
end;

procedure TLearn.AddMemory(const s: TPascalString);
var
  s_In, s_Out: TPascalString;
begin
  s_In := umlGetFirstStr(s, '=');
  s_Out := umlGetLastStr(s, '=');
  AddMemory(s_In.Text, s_Out.Text);
end;

procedure TLearn.AddMemorySamplerWithPCA(const Fast: Boolean; const mr_in: TMemoryRaster; const f_Out: TLearnFloatArray);
var
  f_In: TLearnFloatArray;
begin
  if PCA(Fast, mr_in, f_In) then
    begin
      AddMemory(f_In, f_Out);

      SetLength(f_In, 0);
    end;
end;

procedure TLearn.AddMemorySamplerWithLDA(const Fast: Boolean; const mr_in: TMemoryRaster; const f_Out: TLearnFloatArray);
var
  f_In: TLearnFloatArray;
begin
  if LDA(Fast, mr_in, f_In) then
    begin
      AddMemory(f_In, f_Out);

      SetLength(f_In, 0);
    end;
end;

function TLearn.Train(const TrainDepth: TLearnInteger): Boolean;
var
  p_k         : PLearnKDT;
  p_f         : PDecisionForest;
  p_logit     : PLogitModel;
  p_n         : PMultiLayerPerceptron;
  p_e         : PMLPEnsemble;
  kmIndexOut  : TDynamicIndexArray;
  buff        : TLearnFloat2DArray;
  rInfo       : TLearnInteger;
  mlReport    : TMLPReport;
  IsTerminated: Boolean;
  eBest       : TLearnFloat;
  CVRep       : TMLPCVReport;
  DFRep       : TDFReport;
  logitRep    : TMNLReport;
  bakseed     : TLearnInteger;

  procedure BuildInternalDataInOut;
  var
    I, J: TLearnInteger;
    v   : TLearnFloat;
  begin
    FLastTrainMaxInValue := PLearnMemory(FMemorySource[0])^.m_in[0];
    FLastTrainMaxOutValue := PLearnMemory(FMemorySource[0])^.m_out[0];

    SetLength(buff, FMemorySource.Count, FInLen + FOutLen);
    for I := 0 to FMemorySource.Count - 1 do
      begin
        for J := 0 to FInLen - 1 do
          begin
            v := PLearnMemory(FMemorySource[I])^.m_in[J];
            if v > FLastTrainMaxInValue then
                FLastTrainMaxInValue := v;
            buff[I][J] := v;
          end;

        for J := 0 to FOutLen - 1 do
          begin
            v := PLearnMemory(FMemorySource[I])^.m_out[J];
            if v > FLastTrainMaxOutValue then
                FLastTrainMaxOutValue := v;
            buff[I][FInLen + J] := v;
          end;
      end;
  end;

  procedure BuildInternalDataInOnlyOut1;
  var
    I, J: TLearnInteger;
  begin
    FLastTrainMaxInValue := PLearnMemory(FMemorySource[0])^.m_in[0];
    FLastTrainMaxOutValue := PLearnMemory(FMemorySource[0])^.m_out[0];

    SetLength(buff, FMemorySource.Count, FInLen + 1);
    for I := 0 to FMemorySource.Count - 1 do
      begin
        for J := 0 to FInLen - 1 do
          begin
            buff[I][J] := PLearnMemory(FMemorySource[I])^.m_in[J];
            if buff[I][J] > FLastTrainMaxInValue then
                FLastTrainMaxInValue := buff[I][J];
          end;

        buff[I][FInLen] := PLearnMemory(FMemorySource[I])^.m_out[0];
        if buff[I][FInLen] > FLastTrainMaxOutValue then
            FLastTrainMaxOutValue := buff[I][FInLen];
      end;
  end;

  procedure FreeInternalData;
  begin
    SetLength(buff, 0, 0);
  end;

begin
  Result := False;

  if FIsTraining then
    begin
      FInfo := 'wait Training';
      Exit;
    end;

  if FMemorySource.Count <= 0 then
    begin
      FInfo := 'Out Training set invailed';
      Exit;
    end;

  FIsTraining := True;

  if not FEnabledRandomNumber then
    begin
      bakseed := RandSeed;
      RandSeed := 0;
    end;
  try
    case FLearnType of
      ltKDT:
        begin
          p_k := PLearnKDT(FLearnData);
          p_k^.kdt.Clear;
          {$IFDEF FPC}
          p_k^.kdt.BuildKDTreeM(FMemorySource.Count, nil, @KDInput);
          {$ELSE FPC}
          p_k^.kdt.BuildKDTreeM(FMemorySource.Count, nil, KDInput);
          {$ENDIF FPC}
          FInfo := 'task has been solved';
          Result := True;
        end;
      ltKM:
        begin
          p_k := PLearnKDT(FLearnData);
          p_k^.kdt.Clear;
          if TrainDepth > 1 then
            begin
              {$IFDEF FPC}
              p_k^.kdt.BuildKDTreeWithClusterM(FMemorySource.Count, TrainDepth, 1, kmIndexOut, nil, @KDInput);
              {$ELSE FPC}
              p_k^.kdt.BuildKDTreeWithClusterM(FMemorySource.Count, TrainDepth, 1, kmIndexOut, nil, KDInput);
              {$ENDIF FPC}
            end
          else
            begin
              {$IFDEF FPC}
              p_k^.kdt.BuildKDTreeM(FMemorySource.Count, nil, @KDInput);
              {$ELSE FPC}
              p_k^.kdt.BuildKDTreeM(FMemorySource.Count, nil, KDInput);
              {$ENDIF FPC}
            end;
          FInfo := 'task has been solved';
          Result := True;
        end;
      ltForest:
        begin
          BuildInternalDataInOnlyOut1;
          p_f := PDecisionForest(FLearnData);
          DFBuildRandomDecisionForest(buff, Length(buff), FInLen, 1, 100, 1, rInfo, p_f^, DFRep);
          FreeInternalData;
          case rInfo of
            1: FInfo := 'task has been solved';
            -2: FInfo := 'there is a point with class number outside of [0..NClasses-1]';
            -1: FInfo := 'incorrect parameters was passed (NPoints<1, NVars<1, NClasses<1, NTrees<1, R<=0 or R>1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 1);
        end;
      ltLogit:
        begin
          BuildInternalDataInOnlyOut1;
          p_logit := PLogitModel(FLearnData);
          MNLTrainH(buff, Length(buff), FInLen, Trunc(FLastTrainMaxOutValue) + 1, rInfo, p_logit^, logitRep);
          FreeInternalData;
          case rInfo of
            1: FInfo := 'task has been solved';
            -2: FInfo := 'there is a point with class number outside of [0..NClasses-1]';
            -1: FInfo := 'incorrect parameters was passed (NPoints<NVars+2, NVars<1, NClasses<2)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 1);
        end;
      ltLM:
        begin
          BuildInternalDataInOut;
          p_n := PMultiLayerPerceptron(FLearnData);
          MLPTrainLM(p_n^, buff, Length(buff), 0.001, TrainDepth, rInfo, mlReport);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -9: FInfo := 'internal matrix inverse subroutine failed';
            -2: FInfo := 'there is a point with class number outside of [0..NOut-1]';
            -1: FInfo := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltLM_MT:
        begin
          BuildInternalDataInOut;
          p_n := PMultiLayerPerceptron(FLearnData);
          MLPTrainLM_MT(p_n^, buff, Length(buff), 0.001, TrainDepth, rInfo, mlReport);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -9: FInfo := 'internal matrix inverse subroutine failed';
            -2: FInfo := 'there is a point with class number outside of [0..NOut-1]';
            -1: FInfo := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltLBFGS:
        begin
          BuildInternalDataInOut;
          p_n := PMultiLayerPerceptron(FLearnData);
          IsTerminated := False;
          MLPTrainLBFGS(p_n^, buff, Length(buff), 0.001, TrainDepth, 0.01, 0, rInfo, mlReport, @IsTerminated, eBest);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -8: FInfo := 'if both WStep=0 and MaxIts=0';
            -2: FInfo := 'there is a point with class number outside of [0..NOut-1]';
            -1: FInfo := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltLBFGS_MT:
        begin
          BuildInternalDataInOut;
          p_n := PMultiLayerPerceptron(FLearnData);
          IsTerminated := False;
          MLPTrainLBFGS_MT(p_n^, buff, Length(buff), 0.001, TrainDepth, 0.01, 0, rInfo, mlReport);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -8: FInfo := 'if both WStep=0 and MaxIts=0';
            -2: FInfo := 'there is a point with class number outside of [0..NOut-1]';
            -1: FInfo := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltLBFGS_MT_Mod:
        begin
          BuildInternalDataInOut;
          p_n := PMultiLayerPerceptron(FLearnData);
          IsTerminated := False;
          MLPTrainLBFGS_MT_Mod(p_n^, buff, Length(buff), TrainDepth, 0.01, 2.0, 0, rInfo, mlReport);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -8: FInfo := 'if both WStep=0 and MaxIts=0';
            -2: FInfo := 'there is a point with class number outside of [0..NOut-1]';
            -1: FInfo := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltMonteCarlo:
        begin
          BuildInternalDataInOut;
          p_n := PMultiLayerPerceptron(FLearnData);
          IsTerminated := False;
          MLPTrainMonteCarlo(p_n^, buff, Length(buff), 10, TrainDepth, 0, 1, rInfo, mlReport);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -8: FInfo := 'if both WStep=0 and MaxIts=0';
            -2: FInfo := 'there is a point with class number outside of [0..NOut-1]';
            -1: FInfo := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltLM_Ensemble, ltLM_Ensemble_MT:
        begin
          BuildInternalDataInOut;
          p_e := PMLPEnsemble(FLearnData);
          MLPEBaggingLM(FLearnType = ltLM_Ensemble_MT, p_e^, buff, Length(buff), 0.001, TrainDepth, rInfo, mlReport, CVRep);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -2: FInfo := 'there is a point with class number outside of [0..NClasses-1]';
            -1: FInfo := 'incorrect parameters was passed (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT:
        begin
          BuildInternalDataInOut;
          p_e := PMLPEnsemble(FLearnData);
          MLPEBaggingLBFGS(FLearnType = ltLBFGS_Ensemble_MT, p_e^, buff, Length(buff), 0.001, TrainDepth, 0.01, 0, rInfo, mlReport, CVRep);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -8: FInfo := 'both WStep=0 and MaxIts=0';
            -2: FInfo := 'there is a point with class number outside of [0..NClasses-1]';
            -1: FInfo := 'incorrect parameters was passed (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
    end;
  finally
    FIsTraining := False;
    if not FEnabledRandomNumber then
        RandSeed := bakseed;
  end;
end;

function TLearn.Train: Boolean;
begin
  Result := Train(1);
end;

procedure TLearn.Train_MT;
var
  th: TLearn_th;
begin
  WaitTrain;
  FTrainThreadRuning := True;
  th := TLearn_th.Create;
  th.source := Self;
  th.TrainDepth := 1;
  th.Suspended := False;
end;

procedure TLearn.Train_MT(const TrainDepth: TLearnInteger);
var
  th: TLearn_th;
begin
  WaitTrain;
  FTrainThreadRuning := True;
  th := TLearn_th.Create;
  th.source := Self;
  th.TrainDepth := TrainDepth;
  th.Suspended := False;
end;

procedure TLearn.TrainC(const TrainDepth: TLearnInteger; const OnResult: TLearnState_Call);
var
  th: TLearn_th;
begin
  WaitTrain;
  FTrainThreadRuning := True;
  th := TLearn_th.Create;
  th.source := Self;
  th.OnStateC := OnResult;
  th.TrainDepth := TrainDepth;
  th.Suspended := False;
end;

procedure TLearn.TrainM(const TrainDepth: TLearnInteger; const OnResult: TLearnState_Method);
var
  th: TLearn_th;
begin
  WaitTrain;
  FTrainThreadRuning := True;
  th := TLearn_th.Create;
  th.source := Self;
  th.OnStateM := OnResult;
  th.TrainDepth := TrainDepth;
  th.Suspended := False;
end;

{$IFNDEF FPC}


procedure TLearn.TrainP(const TrainDepth: TLearnInteger; const OnResult: TLearnState_Proc);
var
  th: TLearn_th;
begin
  WaitTrain;
  FTrainThreadRuning := True;
  th := TLearn_th.Create;
  th.source := Self;
  th.OnStateP := OnResult;
  th.TrainDepth := TrainDepth;
  th.Suspended := False;
end;
{$ENDIF FPC}


procedure TLearn.WaitTrain;
begin
  while FTrainThreadRuning do
      CheckThreadSynchronize(1);
end;

function TLearn.process(const ProcessIn, ProcessOut: PLearnFloatArray): Boolean;
var
  p_kd_node: PKDTree_Node;
  I        : TLearnInteger;
  R        : TLearnFloat;
begin
  Result := False;
  if FIsTraining or FTrainThreadRuning then
    begin
      FInfo := 'wait training';
      Exit;
    end;
  if Length(ProcessIn^) <> FInLen then
    begin
      FInfo := 'input length error';
      Exit;
    end;

  case FLearnType of
    ltKDT, ltKM:
      begin
        p_kd_node := PLearnKDT(FLearnData)^.kdt.Search(ProcessIn^);
        if p_kd_node <> nil then
          begin
            SetLength(ProcessOut^, FOutLen);
            CopyPtr(@(PLearnMemory(FMemorySource[p_kd_node^.vec^.index])^.m_out[0]), @ProcessOut^[0], FOutLen * SizeOf(TLearnFloat));
            FInfo := 'successed';
            Result := True;
          end
        else
            FInfo := 'kdTree not inited';
      end;
    ltForest:
      begin
        SetLength(ProcessOut^, FOutLen);
        DFProcess(PDecisionForest(FLearnData)^, ProcessIn^, ProcessOut^);
        FInfo := 'successed';
        Result := True;
      end;
    ltLogit:
      begin
        if Length(PLogitModel(FLearnData)^.w) > 0 then
          begin
            SetLength(ProcessOut^, Trunc(FLastTrainMaxOutValue) + 1);
            MNLProcess(PLogitModel(FLearnData)^, ProcessIn^, ProcessOut^);
            FInfo := 'successed';
            Result := True;
          end;
      end;
    ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo:
      begin
        SetLength(ProcessOut^, FOutLen);
        MLPProcess(PMultiLayerPerceptron(FLearnData)^, ProcessIn^, ProcessOut^);
        FInfo := 'successed';
        Result := True;
      end;
    ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT:
      begin
        SetLength(ProcessOut^, FOutLen);
        MLPEProcess(PMLPEnsemble(FLearnData)^, ProcessIn^, ProcessOut^);
        FInfo := 'successed';
        Result := True;
      end;
  end;
end;

function TLearn.process(const ProcessIn: PLearnFloatArray): string;
var
  ProcessOut: TLearnFloatArray;
begin
  Result := '';
  if not process(ProcessIn, @ProcessOut) then
      Exit;
  Result := TKDTree.KDTreeVec(ProcessOut);
end;

function TLearn.process(const ProcessIn: TLearnFloatArray): string;
begin
  Result := process(PLearnFloatArray(@ProcessIn));
end;

function TLearn.process(const ProcessIn: TPascalString): string;
begin
  Result := process(TKDTree.KDTreeVec(ProcessIn.Text));
end;

function TLearn.processMax(const ProcessIn: TLearnFloatArray): TLearnFloat;
var
  ProcessOut: TLearnFloatArray;
  I         : TLearnInteger;
begin
  Result := 0;
  if not process(@ProcessIn, @ProcessOut) then
      Exit;

  Result := ProcessOut[0];

  if FOutLen > 1 then
    for I := 1 to FOutLen - 1 do
      if ProcessOut[I] > Result then
          Result := ProcessOut[I];

  SetLength(ProcessOut, 0);
end;

function TLearn.processMaxIndex(const ProcessIn: TLearnFloatArray): TLearnInteger;
var
  ProcessOut: TLearnFloatArray;
  k         : TLearnFloat;
  I         : TLearnInteger;
begin
  Result := -1;
  if not process(@ProcessIn, @ProcessOut) then
      Exit;

  k := ProcessOut[0];
  Result := 0;

  if FOutLen > 1 then
    for I := 1 to FOutLen - 1 do
      if ProcessOut[I] > k then
          Result := I;

  SetLength(ProcessOut, 0);
end;

function TLearn.processMin(const ProcessIn: TLearnFloatArray): TLearnFloat;
var
  ProcessOut: TLearnFloatArray;
  I         : TLearnInteger;
begin
  Result := 0;
  if not process(@ProcessIn, @ProcessOut) then
      Exit;

  Result := ProcessOut[0];

  if FOutLen > 1 then
    for I := 1 to FOutLen - 1 do
      if ProcessOut[I] < Result then
          Result := ProcessOut[I];

  SetLength(ProcessOut, 0);
end;

function TLearn.processMinIndex(const ProcessIn: TLearnFloatArray): TLearnInteger;
var
  ProcessOut: TLearnFloatArray;
  k         : TLearnFloat;
  I         : TLearnInteger;
begin
  Result := -1;
  if not process(@ProcessIn, @ProcessOut) then
      Exit;

  k := ProcessOut[0];
  Result := 0;

  if FOutLen > 1 then
    for I := 1 to FOutLen - 1 do
      if ProcessOut[I] < k then
          Result := I;

  SetLength(ProcessOut, 0);
end;

function TLearn.processRF(const ProcessIn: TLearnFloatArray): TLearnFloat;
var
  ProcessOut: TLearnFloatArray;
begin
  Result := 0;
  if not process(@ProcessIn, @ProcessOut) then
      Exit;

  Result := ProcessOut[0];

  SetLength(ProcessOut, 0);
end;

function TLearn.processRF(const ProcessIn: TPascalString): TLearnFloat;
begin
  Result := processRF(TKDTree.KDTreeVec(ProcessIn.Text));
end;

function TLearn.processLF(const ProcessIn: TLearnFloatArray): TLearnFloat;
var
  ProcessOut: TLearnFloatArray;
begin
  Result := 0;
  if not process(@ProcessIn, @ProcessOut) then
      Exit;

  Result := ProcessOut[Length(ProcessOut) - 1];

  SetLength(ProcessOut, 0);
end;

function TLearn.processLF(const ProcessIn: TPascalString): TLearnFloat;
begin
  Result := processLF(TKDTree.KDTreeVec(ProcessIn.Text));
end;

function TLearn.SearchMemoryWithPearson(const ProcessIn: TLearnFloatArray): TLearnInteger;
var
  k, R: TLearnFloat;
  I   : TLearnInteger;
begin
  if Count <= 0 then
    begin
      Result := -1;
      Exit;
    end;

  k := PearsonCorrelation(ProcessIn, GetMemorySource(0)^.m_in, FInLen);
  Result := 0;

  for I := 1 to Count - 1 do
    begin
      R := PearsonCorrelation(ProcessIn, GetMemorySource(I)^.m_in, FInLen);
      if (R <> 0) and (R > k) then
        begin
          k := R;
          Result := I;
        end;
    end;
end;

procedure TLearn.SearchMemoryWithPearson(const ProcessIn: TLearnFloatArray; out List: TLearnIntegerArray);
{$REGION 'Imp'}

type
  TState = record
    k: TLearnFloat;
    index: TLearnInteger;
  end;

  PState = ^TState;

  TStatePtrArray = array of PState;
  TStateArray    = array of TState;

  function SortCompare(const p1, p2: PState): ShortInt; inline;
  begin
    if p1^.k > p2^.k then
        Result := -1
    else if p1^.k < p2^.k then
        Result := 1
    else
        Result := 0;
  end;
  procedure InternalSort(var SortBuffer: TStatePtrArray; l, R: TLearnInteger);
  var
    I, J: TLearnInteger;
    p, t: PState;
  begin
    repeat
      I := l;
      J := R;
      p := SortBuffer[(l + R) shr 1];
      repeat
        while SortCompare(SortBuffer[I], p) < 0 do
            inc(I);
        while SortCompare(SortBuffer[J], p) > 0 do
            Dec(J);
        if I <= J then
          begin
            if I <> J then
              begin
                t := SortBuffer[I];
                SortBuffer[I] := SortBuffer[J];
                SortBuffer[J] := t;
              end;
            inc(I);
            Dec(J);
          end;
      until I > J;
      if l < J then
          InternalSort(SortBuffer, l, J);
      l := I;
    until I >= R;
  end;

var
  buff   : TStateArray;
  buffPtr: TStatePtrArray;

  {$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    buff[pass].k := PearsonCorrelation(ProcessIn, GetMemorySource(pass)^.m_in, FInLen);
    buff[pass].index := pass;
    buffPtr[pass] := @buff[pass];
  end;
{$ENDIF FPC}


var
  I: TLearnInteger;
begin
  if Count < 2 then
      Exit;
  SetLength(buff, Count);
  SetLength(buffPtr, Count);

  {$IFDEF parallel}
  {$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Count - 1);
  {$ELSE}
  TParallel.For(0, Count - 1, procedure(pass: TLearnInteger)
    begin
      buff[pass].k := PearsonCorrelation(ProcessIn, GetMemorySource(pass)^.m_in, FInLen);
      buff[pass].index := pass;
      buffPtr[pass] := @buff[pass];
    end);
  {$ENDIF FPC}
  {$ELSE}
  for I := 1 to Count - 1 do
    begin
      buff[I].k := PearsonCorrelation(ProcessIn, GetMemorySource(I)^.m_in, FInLen);
      buff[I].index := I;
      buffPtr[I] := @buff[I];
    end;
  {$ENDIF parallel}
  // complete sort
  InternalSort(buffPtr, 0, Length(buffPtr) - 1);

  SetLength(List, Count);
  for I := 0 to Count - 1 do
      List[I] := buffPtr[I]^.index;

  SetLength(buff, 0);
  SetLength(buffPtr, 0);
end;
{$ENDREGION 'Imp'}


function TLearn.SearchMemoryWithSpearman(const ProcessIn: TLearnFloatArray): TLearnInteger;
var
  k, R: TLearnFloat;
  I   : TLearnInteger;
begin
  if Count <= 0 then
    begin
      Result := -1;
      Exit;
    end;

  k := SpearmanRankCorrelation(ProcessIn, GetMemorySource(0)^.m_in, FInLen);
  Result := 0;

  for I := 1 to Count - 1 do
    begin
      R := SpearmanRankCorrelation(ProcessIn, GetMemorySource(I)^.m_in, FInLen);
      if (R <> 0) and (R > k) then
        begin
          k := R;
          Result := I;
        end;
    end;
end;

procedure TLearn.SearchMemoryWithSpearman(const ProcessIn: TLearnFloatArray; out List: TLearnIntegerArray);
{$REGION 'Imp'}

type
  TState = record
    k: TLearnFloat;
    index: TLearnInteger;
  end;

  PState = ^TState;

  TStatePtrArray = array of PState;
  TStateArray    = array of TState;

  function SortCompare(const p1, p2: PState): ShortInt; inline;
  begin
    if p1^.k > p2^.k then
        Result := -1
    else if p1^.k < p2^.k then
        Result := 1
    else
        Result := 0;
  end;
  procedure InternalSort(var SortBuffer: TStatePtrArray; l, R: TLearnInteger);
  var
    I, J: TLearnInteger;
    p, t: PState;
  begin
    repeat
      I := l;
      J := R;
      p := SortBuffer[(l + R) shr 1];
      repeat
        while SortCompare(SortBuffer[I], p) < 0 do
            inc(I);
        while SortCompare(SortBuffer[J], p) > 0 do
            Dec(J);
        if I <= J then
          begin
            if I <> J then
              begin
                t := SortBuffer[I];
                SortBuffer[I] := SortBuffer[J];
                SortBuffer[J] := t;
              end;
            inc(I);
            Dec(J);
          end;
      until I > J;
      if l < J then
          InternalSort(SortBuffer, l, J);
      l := I;
    until I >= R;
  end;

var
  buff   : TStateArray;
  buffPtr: TStatePtrArray;

  {$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    buff[pass].k := SpearmanRankCorrelation(ProcessIn, GetMemorySource(pass)^.m_in, FInLen);
    buff[pass].index := pass;
    buffPtr[pass] := @buff[pass];
  end;
{$ENDIF FPC}


var
  I: TLearnInteger;
begin
  if Count < 2 then
      Exit;
  SetLength(buff, Count);
  SetLength(buffPtr, Count);

  {$IFDEF parallel}
  {$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Count - 1);
  {$ELSE}
  TParallel.For(0, Count - 1, procedure(pass: TLearnInteger)
    begin
      buff[pass].k := SpearmanRankCorrelation(ProcessIn, GetMemorySource(pass)^.m_in, FInLen);
      buff[pass].index := pass;
      buffPtr[pass] := @buff[pass];
    end);
  {$ENDIF FPC}
  {$ELSE}
  for I := 1 to Count - 1 do
    begin
      buff[I].k := SpearmanRankCorrelation(ProcessIn, GetMemorySource(I)^.m_in, FInLen);
      buff[I].index := I;
      buffPtr[I] := @buff[I];
    end;
  {$ENDIF parallel}
  // complete sort
  InternalSort(buffPtr, 0, Length(buffPtr) - 1);

  SetLength(List, Count);
  for I := 0 to Count - 1 do
      List[I] := buffPtr[I]^.index;

  SetLength(buff, 0);
  SetLength(buffPtr, 0);
end;
{$ENDREGION 'Imp'}


function TLearn.SearchMemoryWithKDTree(const ProcessIn: TLearnFloatArray): TLearnInteger;
var
  rD: Double;
  rN: PKDTree_Node;
  I : TLearnInteger;
begin
  if Count <= 0 then
    begin
      Result := -1;
      Exit;
    end;

  if FLearnType in [ltKDT, ltKM] then
    begin
      rN := PLearnKDT(FLearnData)^.kdt.Search(ProcessIn, rD);
      if rN <> nil then
          Result := rN^.vec^.index;
    end
  else
      raiseInfo('only work in LearnType = ltKDT or ltKM');
end;

procedure TLearn.SearchMemoryWithKDTree(const ProcessIn: TLearnFloatArray; out List: TLearnIntegerArray);
var
  rD: Double;
  rC: NativeInt;
  rN: TCoreClassList;
  I : TLearnInteger;
begin
  if Count <= 0 then
    begin
      Exit;
    end;

  if FLearnType in [ltKDT, ltKM] then
    begin
      rN := TCoreClassList.Create;
      PLearnKDT(FLearnData)^.kdt.Search(ProcessIn, rD, rC, rN);
      SetLength(List, rN.Count);
      for I := 0 to rN.Count - 1 do
          List[I] := PKDTree_Node(rN[I])^.vec^.index;
      DisposeObject(rN);
    end
  else
      raiseInfo('only work in LearnType = ltKDT or ltKM');
end;

procedure TLearn.SaveToDF(df: TDataFrameEngine);
var
  ar     : TDataFrameArrayDouble;
  I, J   : TLearnInteger;
  buff   : TLearnFloatArray;
  buffLen: TLearnInteger;
  m64    : TMemoryStream64;
begin
  df.WriteInt64(FInLen);
  df.WriteInt64(FOutLen);
  df.WriteByte(Byte(FLearnType));
  df.WriteBool(FEnabledRandomNumber);
  df.WriteDouble(FLastTrainMaxInValue);
  df.WriteDouble(FLastTrainMaxOutValue);

  ar := df.WriteArrayDouble;
  for I := 0 to FMemorySource.Count - 1 do
    begin
      for J := 0 to FInLen - 1 do
          ar.Add(PLearnMemory(FMemorySource[I])^.m_in[J]);
      for J := 0 to FOutLen - 1 do
          ar.Add(PLearnMemory(FMemorySource[I])^.m_out[J]);
    end;

  case FLearnType of
    ltKDT, ltKM:
      begin
        if PLearnKDT(FLearnData)^.kdt.Count > 0 then
          begin
            m64 := TMemoryStream64.Create;
            PLearnKDT(FLearnData)^.kdt.SaveToStream(m64);
            df.WriteStream(m64);
            DisposeObject(m64);
          end;
      end;
    ltForest:
      begin
        if Length(PDecisionForest(FLearnData)^.Trees) > 0 then
          begin
            DFSerialize(PDecisionForest(FLearnData)^, buff, buffLen);
            ar := df.WriteArrayDouble;
            for I := 0 to buffLen - 1 do
                ar.Add(buff[I]);
          end;
      end;
    ltLogit:
      begin
        if Length(PLogitModel(FLearnData)^.w) > 0 then
          begin
            MNLSerialize(PLogitModel(FLearnData)^, buff, buffLen);
            ar := df.WriteArrayDouble;
            for I := 0 to buffLen - 1 do
                ar.Add(buff[I]);
          end;
      end;
    ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo:
      begin
        if Length(PMultiLayerPerceptron(FLearnData)^.Neurons) > 0 then
          begin
            MLPSerialize(PMultiLayerPerceptron(FLearnData)^, buff, buffLen);
            ar := df.WriteArrayDouble;
            for I := 0 to buffLen - 1 do
                ar.Add(buff[I]);
          end;
      end;
    ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT:
      begin
        if Length(PMLPEnsemble(FLearnData)^.DFDNET) > 0 then
          begin
            MLPESerialize(PMLPEnsemble(FLearnData)^, buff, buffLen);
            ar := df.WriteArrayDouble;
            for I := 0 to buffLen - 1 do
                ar.Add(buff[I]);
          end;
      end;
  end;
end;

procedure TLearn.LoadFromDF(df: TDataFrameEngine);
var
  ar  : TDataFrameArrayDouble;
  I, J: TLearnInteger;
  plm : PLearnMemory;
  buff: TLearnFloatArray;
  m64 : TMemoryStream64;
begin
  Clear;

  FInLen := df.Reader.ReadInt64;
  FOutLen := df.Reader.ReadInt64;
  FLearnType := TLearnType(df.Reader.ReadByte);
  FEnabledRandomNumber := df.Reader.ReadBool;
  FLastTrainMaxInValue := df.Reader.ReadDouble;
  FLastTrainMaxOutValue := df.Reader.ReadDouble;

  ar := df.Reader.ReadArrayDouble;

  I := 0;
  while I < ar.Count do
    begin
      new(plm);
      SetLength(plm^.m_in, FInLen);
      SetLength(plm^.m_out, FOutLen);
      FMemorySource.Add(plm);

      J := 0;
      while J < FInLen do
        begin
          plm^.m_in[J] := ar[I];
          inc(J);
          inc(I);
        end;

      J := 0;
      while J < FOutLen do
        begin
          plm^.m_out[J] := ar[I];
          inc(J);
          inc(I);
        end;
    end;

  if df.Reader.IsEnd then
    begin
      Train;
      Exit;
    end;

  case FLearnType of
    ltKDT, ltKM:
      begin
        m64 := TMemoryStream64.Create;
        df.Reader.ReadStream(m64);
        m64.Position := 0;
        try
            PLearnKDT(FLearnData)^.kdt.LoadFromStream(m64);
        except
            Train;
        end;
        DisposeObject(m64);
      end;
    ltForest:
      begin
        ar := df.Reader.ReadArrayDouble;
        SetLength(buff, ar.Count);
        for I := 0 to ar.Count - 1 do
            buff[I] := ar[I];

        try
            DFUnserialize(buff, PDecisionForest(FLearnData)^);
        except
            Train;
        end;
        SetLength(buff, 0);
      end;
    ltLogit:
      begin
        ar := df.Reader.ReadArrayDouble;
        SetLength(buff, ar.Count);
        for I := 0 to ar.Count - 1 do
            buff[I] := ar[I];

        try
            MNLUnserialize(buff, PLogitModel(FLearnData)^);
        except
            Train;
        end;
        SetLength(buff, 0);
      end;
    ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo:
      begin
        ar := df.Reader.ReadArrayDouble;
        SetLength(buff, ar.Count);
        for I := 0 to ar.Count - 1 do
            buff[I] := ar[I];

        try
            MLPUNSerialize(buff, PMultiLayerPerceptron(FLearnData)^);
        except
            Train;
        end;
        SetLength(buff, 0);
      end;
    ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT:
      begin
        ar := df.Reader.ReadArrayDouble;
        SetLength(buff, ar.Count);
        for I := 0 to ar.Count - 1 do
            buff[I] := ar[I];

        try
            MLPEUNSerialize(buff, PMLPEnsemble(FLearnData)^);
        except
            Train;
        end;
        SetLength(buff, 0);
      end;
  end;
end;

procedure TLearn.SaveToStream(stream: TCoreClassStream);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  SaveToDF(de);

  de.EncodeTo(stream, True);
  DisposeObject(de);
end;

procedure TLearn.LoadFromStream(stream: TCoreClassStream);
var
  de: TDataFrameEngine;
begin
  Clear;

  de := TDataFrameEngine.Create;
  de.DecodeFrom(stream, True);

  LoadFromDF(de);

  DisposeObject(de);
end;

{$IFNDEF FPC}


procedure TLearn.SaveToJsonStream(stream: TCoreClassStream);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  SaveToDF(de);

  de.EncodeAsPublicJson(stream);
  DisposeObject(de);
end;

procedure TLearn.LoadFromJsonStream(stream: TCoreClassStream);
var
  de: TDataFrameEngine;
begin
  Clear;

  de := TDataFrameEngine.Create;
  de.DecodeFromJson(stream);

  LoadFromDF(de);

  DisposeObject(de);
end;
{$ENDIF FPC}


function TLearn.LDA(const buff: TLearnMatrix; const NPoints, NVars, NClasses: TLearnInteger; var w: TLearnMatrix): TLearnInteger;
begin
  FisherLDAN(buff, NPoints, NVars, NClasses, Result, w);
end;

function TLearn.LDA(const buff: TLearnMatrix; const NPoints, NVars, NClasses: TLearnInteger; var w: TLearnVector): TLearnInteger;
begin
  FisherLDA(buff, NPoints, NVars, NClasses, Result, w);
end;

function TLearn.LDA(const Fast: Boolean; const mr: TMemoryRaster; var output: TLearnMatrix): Boolean;
var
  k        : TLearnFloat;
  mrblur, N: TMemoryRaster;
  I, J     : TLearnInteger;
  buff     : TLearnMatrix;
  rInfo    : TLearnInteger;
begin
  k := FInLen / mr.Width;

  N := TMemoryRaster.Create;
  if (not Fast) and (k < 1.0) then
    begin
      mrblur := TMemoryRaster.Create;
      // preprocess liner zoom
      GrayscaleBlur(mr, mrblur, mr.Width / FInLen, mr.BoundsRect);
      // zoom
      N.ZoomFrom(mrblur, FInLen, Round(mrblur.Height * k));
      DisposeObject(mrblur);
    end
  else
    begin
      // zoom
      N.ZoomFrom(mr, FInLen, Round(mr.Height * k));
      if not Fast then
          GrayscaleBlur(N, FInLen / mr.Width, N.BoundsRect);
    end;

  SetLength(buff, N.Height, FInLen + 1);

  for I := 0 to N.Height - 1 do
    begin
      for J := 0 to FInLen - 1 do
          buff[I, J] := PRasterColorEntry(N.PixelPtr[J, I])^.R;
      buff[I, FInLen] := 0;
    end;

  FisherLDAN(buff, N.Height, FInLen, 2, rInfo, output);

  case rInfo of
    1: FInfo := 'task is solved';
    2: FInfo := 'there was a multicollinearity in training set, but task has been solved';
    -1: FInfo := 'incorrect parameters was passed (NPoints<0, NVars<1, NClasses<2)';
    -2: FInfo := 'there is a point with class number outside of [0..NClasses-1]';
    -4: FInfo := 'internal EVD subroutine hasn''''t converged';
    else FInfo := 'unknow';
  end;

  Result := rInfo in [1, 2];

  SetLength(buff, 0, 0);
  DisposeObject(N);
end;

function TLearn.LDA(const Fast: Boolean; const mr: TMemoryRaster; var output: TLearnVector): Boolean;
var
  M: TLearnMatrix;
  I: TLearnInteger;
begin
  Result := LDA(Fast, mr, M);
  if Result then
    begin
      SetLength(output, FInLen);
      for I := 0 to FInLen - 1 do
          output[I] := M[I][I];
    end;
end;

function TLearn.LDA(const Fast: Boolean; const mr1, mr2: TMemoryRaster): TLearnFloat;
var
  V1, V2: TLearnVector;
begin
  Result := -1;
  if LDA(Fast, mr1, V1) and LDA(Fast, mr2, V2) then
      Result := SpearmanRankCorrelation(V1, V2, FInLen);
  SetLength(V1, 0);
  SetLength(V2, 0);
end;

function TLearn.PCA(const buff: TLearnMatrix; const NPoints, NVars: TLearnInteger; var v: TLearnMatrix): TLearnInteger;
var
  s: TLearnVector;
begin
  PCABuildBasis(buff, NPoints, NVars, Result, s, v);
  SetLength(s, 0);
end;

function TLearn.PCA(const Fast: Boolean; const mr: TMemoryRaster; var output: TLearnMatrix): Boolean;
var
  k        : TLearnFloat;
  mrblur, N: TMemoryRaster;
  I, J     : TLearnInteger;
  buff     : TLearnMatrix;
  basisVec : TLearnVector;
  rInfo    : TLearnInteger;
begin
  Result := False;

  k := FInLen / mr.Width;

  N := TMemoryRaster.Create;
  if (not Fast) and (k < 1.0) then
    begin
      mrblur := TMemoryRaster.Create;
      // preprocess liner zoom
      GrayscaleBlur(mr, mrblur, mr.Width / FInLen, mr.BoundsRect);
      // zoom
      N.ZoomFrom(mrblur, FInLen, Round(mrblur.Height * k));
      DisposeObject(mrblur);
    end
  else
    begin
      // zoom
      N.ZoomFrom(mr, FInLen, Round(mr.Height * k));
      if not Fast then
          GrayscaleBlur(N, FInLen / mr.Width, N.BoundsRect);
    end;

  SetLength(buff, N.Height, FInLen);

  for I := 0 to N.Height - 1 do
    for J := 0 to N.Width - 1 do
        buff[I, J] := PRasterColorEntry(N.PixelPtr[J, I])^.R;

  PCABuildBasis(buff, N.Height, N.Width, rInfo, basisVec, output);

  case rInfo of
    1: FInfo := 'task is solved';
    -1: FInfo := 'wrong parameters has been passed (NPoints<0, NVars<1)';
    -4: FInfo := 'SVD subroutine haven''''t converged';
    else FInfo := 'unknow';
  end;

  Result := rInfo = 1;

  SetLength(buff, 0, 0);
  SetLength(basisVec, 0);
  DisposeObject(N);
end;

function TLearn.PCA(const Fast: Boolean; const mr: TMemoryRaster; var output: TLearnVector): Boolean;
var
  M: TLearnMatrix;
  I: TLearnInteger;
begin
  Result := PCA(Fast, mr, M);
  if Result then
    begin
      SetLength(output, FInLen);
      for I := 0 to FInLen - 1 do
          output[I] := M[I][I];

      SetLength(M, 0, 0);
    end;
end;

function TLearn.PCA(const Fast: Boolean; const mr1, mr2: TMemoryRaster): TLearnFloat;
var
  V1, V2: TLearnVector;
begin
  Result := -1;
  if PCA(Fast, mr1, V1) and PCA(Fast, mr2, V2) then
      Result := SpearmanRankCorrelation(V1, V2, FInLen);
  SetLength(V1, 0);
  SetLength(V2, 0);
end;

function TLearn.KMeans(const source: TKMFloat2DArray; const NVars, k: TLearnInteger; var KArray: TKMFloat2DArray; var kIndex: TKMIntegerArray): Boolean;
var
  bakseed: TLearnInteger;
begin
  if not FEnabledRandomNumber then
    begin
      bakseed := RandSeed;
      RandSeed := 0;
    end;
  try
      Result := KMeansCluster(source, NVars, k, 1, KArray, kIndex) = 1;
  finally
    if not FEnabledRandomNumber then
        RandSeed := bakseed;
  end;
end;

initialization

end.
