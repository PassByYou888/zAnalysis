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

{$I zDefine.inc}

interface

uses CoreClasses, UnicodeMixedLib, PascalStrings, MemoryRaster, MemoryStream64, DataFrameEngine,
  KDTree, LearnTypes;

{$REGION 'Class'}


type
  TLearn = class;

  TLearnState_Call             = procedure(const LSender: TLearn; const state: Boolean);
  TLearnState_Method           = procedure(const LSender: TLearn; const state: Boolean) of object;
{$IFNDEF FPC} TLearnState_Proc = reference to procedure(const LSender: TLearn; const state: Boolean); {$ENDIF}

  TLearn = class(TCoreClassInterfacedObject)
  public type
    TLearnMemory = packed record
      m_in, m_out: TLVec;
    end;

    PLearnMemory = ^TLearnMemory;
  private type
    THideLayerDepth = (hld0, hld1, hld2);

    TLearnKDT = packed record
      kdt: TKDTree;
    end;

    PLearnKDT = ^TLearnKDT;
  private
    FEnabledRandomNumber: Boolean;
    FInLen, FOutLen: TLInt;
    FMemorySource: TCoreClassList;
    FLearnType: TLearnType;
    FLearnData: Pointer;
    FClassifier: Boolean;
    FHideLayerDepth: THideLayerDepth;
    FLastTrainMaxInValue, FLastTrainMaxOutValue: TLFloat;
    FInfo: SystemString;
    FIsTraining: Boolean;
    FTrainThreadRuning: Boolean;
    FUserData: Pointer;
    FUserObject: TCoreClassObject;

    procedure KDInput(const IndexFor: NativeInt; var source: TKDTree_Source; const Data: Pointer);

    procedure FreeLearnData;
    procedure CreateLearnData(const isTrainTime: Boolean);
  public
    // regression style
    class function CreateRegression(const lt: TLearnType; const InDataLen, OutDataLen: TLInt): TLearn;
    // regression style of level 1
    class function CreateRegression1(const lt: TLearnType; const InDataLen, OutDataLen: TLInt): TLearn;
    // regression style of level 2
    class function CreateRegression2(const lt: TLearnType; const InDataLen, OutDataLen: TLInt): TLearn;

    // classifier style
    class function CreateClassifier(const lt: TLearnType; const InDataLen: TLInt): TLearn;
    // classifier style of level 1
    class function CreateClassifier1(const lt: TLearnType; const InDataLen: TLInt): TLearn;
    // classifier style of level 2
    class function CreateClassifier2(const lt: TLearnType; const InDataLen: TLInt): TLearn;

    // picture classifier style
    class function CreatePictureClassifier(const lt: TLearnType; const SamplerWidth: TLInt):TLearn;

    // regression style with fast Histogram of Oriented Gradient
    class function CreateHOGRegression(const lt: TLearnType; const OutDataLen: TLInt): TLearn;
    // classifier style with fast Histogram of Oriented Gradient
    class function CreateHOGClassifier(const lt: TLearnType): TLearn;

    constructor Create; virtual;
    destructor Destroy; override;

    { * random number * }
    property EnabledRandomNumber: Boolean read FEnabledRandomNumber write FEnabledRandomNumber;

    { * clear * }
    procedure Clear;

    { * parameter * }
    function Count: TLInt;
    property InLen: TLInt read FInLen;
    property OutLen: TLInt read FOutLen;
    property LearnType: TLearnType read FLearnType;
    property Info: SystemString read FInfo;
    property TrainThreadRuning: Boolean read FTrainThreadRuning;
    function GetMemorySource(const index: TLInt): PLearnMemory;
    property MemorySource[const index: TLInt]: PLearnMemory read GetMemorySource; default;
    property LastTrainMaxInValue: TLFloat read FLastTrainMaxInValue;
    property LastTrainMaxOutValue: TLFloat read FLastTrainMaxOutValue;

    { * user parameter * }
    property UserData: Pointer read FUserData write FUserData;
    property UserObject: TCoreClassObject read FUserObject write FUserObject;

    { * sampler * }
    procedure AddMemory(const f_In, f_Out: TLVec); overload;
    procedure AddMemory(const s_In, s_Out: SystemString); overload;
    procedure AddMemory(const s: TPascalString); overload;
    procedure AddSampler(const f_In, f_Out: TLVec); overload;
    procedure AddSampler(const s_In, s_Out: SystemString); overload;
    procedure AddSampler(const s: TPascalString); overload;
    procedure AddMatrix(const m_in: TLMatrix; const f_Out: TLVec);

    { * normal train * }
    function Train(const TrainDepth: TLInt): Boolean; overload;
    function Train: Boolean; overload;
    { * train with thread * }
    procedure Train_MT; overload;
    procedure Train_MT(const TrainDepth: TLInt); overload;
    procedure TrainC(const TrainDepth: TLInt; const OnResult: TLearnState_Call);
    procedure TrainM(const TrainDepth: TLInt; const OnResult: TLearnState_Method);
{$IFNDEF FPC} procedure TrainP(const TrainDepth: TLInt; const OnResult: TLearnState_Proc); {$ENDIF FPC}
    //
    // wait thread
    procedure WaitTrain;
    //
    // data input/output
    function process(const p_in, p_out: PLVec): Boolean; overload;
    function process(const ProcessIn: PLVec): SystemString; overload;
    function process(const ProcessIn: TLVec): SystemString; overload;
    function process(const ProcessIn: TPascalString): SystemString; overload;
    function processMatrix(const p_in: PLMatrix; const p_out: PLVec): Boolean; overload;
    // result max value
    function processMax(const ProcessIn: TLVec): TLFloat; overload;
    function processMax(const ProcessIn: TLMatrix): TLFloat; overload;
    // result max index
    function processMaxIndex(const ProcessIn: TLVec): TLInt; overload;
    function processMaxIndex(const ProcessIn: TLMatrix): TLInt; overload;
    // result min value
    function processMin(const ProcessIn: TLVec): TLFloat; overload;
    function processMin(const ProcessIn: TLMatrix): TLFloat; overload;
    // result min index
    function processMinIndex(const ProcessIn: TLVec): TLInt; overload;
    function processMinIndex(const ProcessIn: TLMatrix): TLInt; overload;
    // result first value
    function processFV(const ProcessIn: TLVec): TLFloat; overload;
    function processFV(const ProcessIn: TLMatrix): TLFloat; overload;
    function processFV(const ProcessIn: TPascalString): TLFloat; overload;
    // result last value
    function processLV(const ProcessIn: TLVec): TLFloat; overload;
    function processLV(const ProcessIn: TLMatrix): TLFloat; overload;
    function processLV(const ProcessIn: TPascalString): TLFloat; overload;

    // search with Pearson
    function SearchMemoryWithPearson(const ProcessIn: TLVec): TLInt; overload;
    // search with Pearson - parallel support
    procedure SearchMemoryWithPearson(const ProcessIn: TLVec; out List: TLIVec); overload;

    // search with Spearman
    function SearchMemoryWithSpearman(const ProcessIn: TLVec): TLInt; overload;
    // search with Spearman - parallel support
    procedure SearchMemoryWithSpearman(const ProcessIn: TLVec; out List: TLIVec); overload;

    // search with euclidean metric
    function SearchMemoryWithDistance(const ProcessIn: TLVec): TLInt; overload;
    // search with euclidean metric - parallel support
    procedure SearchMemoryWithDistance(const ProcessIn: TLVec; out List: TLIVec); overload;

    { * fast binary store support * }
    procedure SaveToDF(df: TDataFrameEngine);
    procedure LoadFromDF(df: TDataFrameEngine);

    { stream store support }
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

{$IFNDEF FPC}
    { * json store support * }
    procedure SaveToJsonStream(stream: TCoreClassStream);
    procedure LoadFromJsonStream(stream: TCoreClassStream);
{$ENDIF FPC}
  end;

{$ENDREGION 'Class'}

{$REGION 'LearnAPI'}


procedure LAdd(var f: TLFloat; const Value: TLFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure LSub(var f: TLFloat; const Value: TLFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure LMul(var f: TLFloat; const Value: TLFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure LDiv(var f: TLFloat; const Value: TLFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LSafeDivF(const s, d: TLFloat): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure LSetVec(var v: TLVec; const VDef: TLFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure LSetVec(var v: TLIVec; const VDef: TLInt); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure LSetVec(var v: TLBVec; const VDef: Boolean); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure LSetMatrix(var m: TLMatrix; const VDef: TLFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure LSetMatrix(var m: TLIMatrix; const VDef: TLInt); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure LSetMatrix(var m: TLBMatrix; const VDef: Boolean); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LVecCopy(const v: TLVec): TLVec; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LVecCopy(const v: TLIVec): TLIVec; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LVecCopy(const v: TLBVec): TLBVec; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LMatrixCopy(const v: TLMatrix): TLMatrix; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LMatrixCopy(const v: TLIMatrix): TLIMatrix; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LMatrixCopy(const v: TLBMatrix): TLBMatrix; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LVec(const veclen: TLInt; const VDef: TLFloat): TLVec; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LVec(const veclen: TLInt): TLVec; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LVec(const v: TLVec): TPascalString; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LVec(const m: TLMatrix; const veclen: TLInt): TLVec; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LVec(const m: TLMatrix): TLVec; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LVec(const s: TPascalString; const veclen: TLInt): TLVec; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LVec(const v: TLVec; const ShortFloat: Boolean): TPascalString; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LVec(const m: TLBMatrix; const veclen: TLInt): TLBVec; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LVec(const m: TLBMatrix): TLBVec; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LVec(const m: TLIMatrix; const veclen: TLInt): TLIVec; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LVec(const m: TLIMatrix): TLIVec; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LSpearmanVec(const m: TLMatrix; const veclen: TLInt): TLVec; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LAbsMaxVec(const v: TLVec): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LMaxVec(const v: TLVec): TLFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LMaxVec(const v: TLIVec): TLInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LMaxVec(const v: TLMatrix): TLFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LMaxVec(const v: TLIMatrix): TLInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LMinVec(const v: TLVec): TLFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LMinVec(const v: TLIVec): TLInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LMinVec(const v: TLMatrix): TLFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LMinVec(const v: TLIMatrix): TLInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LMaxVecIndex(const v: TLVec): TLInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LMinVecIndex(const v: TLVec): TLInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LDistance(const v1, v2: TLVec): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LHamming(const v1, v2: TLVec): TLInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LHamming(const v1, v2: TLIVec): TLInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure LClampF(var AValue: TLFloat; const AMin, AMax: TLFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure LClampI(var AValue: TLInt; const AMin, AMax: TLInt); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LClamp(const AValue: TLFloat; const AMin, AMax: TLFloat): TLFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LClamp(const AValue: TLInt; const AMin, AMax: TLInt): TLInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

{ * sampler support * }
procedure LFeatureSamplerWithSift(const mr: TMemoryRaster; ln: TLearn; lOut: TLVec);
function LMatrixSamplerWithHOG(const mr: TMemoryRaster): TLMatrix;
function LMatrixSampler(const mr: TMemoryRaster): TLMatrix; overload;
function LMatrixSampler(const SamplerSize: TLInt; const mr: TMemoryRaster): TLMatrix; overload;
function LMatrixSampler(const Antialiasing: Boolean; const SamplerSize: TLInt; const mr: TMemoryRaster): TLMatrix; overload;
procedure LZoomMatrix(var source, dest: TLMatrix; const DestWidth, DestHeight: TLInt); overload;
procedure LZoomMatrix(var source, dest: TLIMatrix; const DestWidth, DestHeight: TLInt); overload;
procedure LZoomMatrix(var source, dest: TLBMatrix; const DestWidth, DestHeight: TLInt); overload;
procedure LSampler(var source: TLBMatrix; const dest: TMemoryRaster); overload;
procedure LSampler(var source: TLMatrix; const MaxF: TLFloat; const dest: TMemoryRaster); overload;
procedure LSampler(var source: TLMatrix; const dest: TMemoryRaster); overload;
procedure LSampler(var source: TLIMatrix; const MaxI: TLInt; const dest: TMemoryRaster); overload;
procedure LSampler(var source: TLIMatrix; const dest: TMemoryRaster); overload;

{ matrix as stream }
procedure LSaveMatrix(var source: TLMatrix; dest: TCoreClassStream); overload;
procedure LLoadMatrix(source: TCoreClassStream; var dest: TLMatrix); overload;
procedure LSaveMatrix(var source: TLIMatrix; dest: TCoreClassStream); overload;
procedure LLoadMatrix(source: TCoreClassStream; var dest: TLIMatrix); overload;
procedure LSaveMatrix(var source: TLBMatrix; dest: TCoreClassStream); overload;
procedure LLoadMatrix(source: TCoreClassStream; var dest: TLBMatrix); overload;

{ save sampler as viewer }
procedure LSaveSampler(var source: TLBMatrix; const fn: SystemString); overload;
procedure LSaveSampler(var source: TLIMatrix; const fn: SystemString); overload;
procedure LSaveSampler(var source: TLIMatrix; const MaxI: TLInt; const fn: SystemString); overload;
procedure LSaveSampler(var source: TLMatrix; const fn: SystemString); overload;
procedure LSaveSampler(var source: TLMatrix; const MaxF: TLFloat; const fn: SystemString); overload;

{ * linear discriminant analysis support * }
function LDA(const m: TLMatrix; const cv: TLVec; const Nclass: TLInt; var sInfo: SystemString; var output: TLMatrix): Boolean; overload;
function LDA(const m: TLMatrix; const cv: TLVec; const Nclass: TLInt; var sInfo: SystemString; var output: TLVec): Boolean; overload;
function LDA(const Fast: Boolean; const SamplerSize: TLInt; const mr: TMemoryRaster; var sInfo: SystemString; var output: TLMatrix): Boolean; overload;

{ * principal component analysis support * }
function PCA(const buff: TLMatrix; const NPoints, NVars: TLInt; var v: TLMatrix): TLInt; overload;
function PCA(const Fast: Boolean; const SamplerSize: TLInt; const mr: TMemoryRaster; var sInfo: SystemString; var output: TLMatrix): Boolean; overload;

{ * k-means++ clusterization support * }
function KMeans(const source: TLMatrix; const NVars, k: TLInt; var KArray: TLMatrix; var kIndex: TLIVec): Boolean;

{ * init Matrix * }
function LMatrix(const l1, l2: TLInt): TLMatrix; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LBMatrix(const l1, l2: TLInt): TLBMatrix; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LIMatrix(const l1, l2: TLInt): TLIMatrix; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

{$ENDREGION 'LearnAPI'}

{$REGION 'FloatAPI'}
function AbsReal(X: TLFloat): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AbsInt(I: TLInt): TLInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RandomReal(): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RandomInteger(I: TLInt): TLInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Sign(X: TLFloat): TLInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_Sqr(X: TLFloat): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function DynamicArrayCopy(const A: TLIVec): TLIVec; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DynamicArrayCopy(const A: TLVec): TLVec; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DynamicArrayCopy(const A: TLComplexVec): TLComplexVec; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DynamicArrayCopy(const A: TLBVec): TLBVec; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function DynamicArrayCopy(const A: TLIMatrix): TLIMatrix; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DynamicArrayCopy(const A: TLMatrix): TLMatrix; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DynamicArrayCopy(const A: TLComplexMatrix): TLComplexMatrix; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DynamicArrayCopy(const A: TLBMatrix): TLBMatrix; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function AbsComplex(const Z: TLComplex): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Conj(const Z: TLComplex): TLComplex; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CSqr(const Z: TLComplex): TLComplex; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function C_Complex(const X: TLFloat): TLComplex; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_Opposite(const Z: TLComplex): TLComplex; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_Add(const Z1: TLComplex; const Z2: TLComplex): TLComplex; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_Mul(const Z1: TLComplex; const Z2: TLComplex): TLComplex; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_AddR(const Z1: TLComplex; const R: TLFloat): TLComplex; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_MulR(const Z1: TLComplex; const R: TLFloat): TLComplex; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_Sub(const Z1: TLComplex; const Z2: TLComplex): TLComplex; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_SubR(const Z1: TLComplex; const R: TLFloat): TLComplex; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_RSub(const R: TLFloat; const Z1: TLComplex): TLComplex; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_Div(const Z1: TLComplex; const Z2: TLComplex): TLComplex; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_DivR(const Z1: TLComplex; const R: TLFloat): TLComplex; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_RDiv(const R: TLFloat; const Z2: TLComplex): TLComplex; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_Equal(const Z1: TLComplex; const Z2: TLComplex): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_NotEqual(const Z1: TLComplex; const Z2: TLComplex): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_EqualR(const Z1: TLComplex; const R: TLFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_NotEqualR(const Z1: TLComplex; const R: TLFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function APVDotProduct(v1: PLFloat; I11, I12: TLInt; v2: PLFloat; I21, I22: TLInt): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVMove(VDst: PLFloat; I11, I12: TLInt; VSrc: PLFloat; I21, I22: TLInt); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVMove(VDst: PLFloat; I11, I12: TLInt; VSrc: PLFloat; I21, I22: TLInt; s: TLFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVMoveNeg(VDst: PLFloat; I11, I12: TLInt; VSrc: PLFloat; I21, I22: TLInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVAdd(VDst: PLFloat; I11, I12: TLInt; VSrc: PLFloat; I21, I22: TLInt); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVAdd(VDst: PLFloat; I11, I12: TLInt; VSrc: PLFloat; I21, I22: TLInt; s: TLFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVSub(VDst: PLFloat; I11, I12: TLInt; VSrc: PLFloat; I21, I22: TLInt); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVSub(VDst: PLFloat; I11, I12: TLInt; VSrc: PLFloat; I21, I22: TLInt; s: TLFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVMul(VOp: PLFloat; I1, I2: TLInt; s: TLFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVFillValue(VOp: PLFloat; I1, I2: TLInt; s: TLFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}

function AP_Float(X: TLFloat): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function AP_FP_Eq(X: TLFloat; Y: TLFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_FP_NEq(X: TLFloat; Y: TLFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_FP_Less(X: TLFloat; Y: TLFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_FP_Less_Eq(X: TLFloat; Y: TLFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_FP_Greater(X: TLFloat; Y: TLFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_FP_Greater_Eq(X: TLFloat; Y: TLFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure TagSort(var A: TLVec; const N: TLInt; var P1: TLIVec; var P2: TLIVec); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure TagSortFastI(var A: TLVec; var B: TLIVec; N: TLInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure TagSortFastR(var A: TLVec; var B: TLVec; N: TLInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure TagSortFast(var A: TLVec; const N: TLInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure TagHeapPushI(var A: TLVec; var B: TLIVec; var N: TLInt; const VA: TLFloat; const VB: TLInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure TagHeapReplaceTopI(var A: TLVec; var B: TLIVec; const N: TLInt; const VA: TLFloat; const VB: TLInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure TagHeapPopI(var A: TLVec; var B: TLIVec; var N: TLInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}
{$ENDREGION 'FloatAPI'}

{$REGION 'LowLevelMatrix'}
{ matrix base }
function VectorNorm2(const X: TLVec; const I1, I2: TLInt): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function VectorIdxAbsMax(const X: TLVec; const I1, I2: TLInt): TLInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ColumnIdxAbsMax(const X: TLMatrix; const I1, I2, J: TLInt): TLInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RowIdxAbsMax(const X: TLMatrix; const J1, J2, I: TLInt): TLInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UpperHessenberg1Norm(const A: TLMatrix; const I1, I2, J1, J2: TLInt; var WORK: TLVec): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CopyMatrix(const A: TLMatrix; const IS1, IS2, JS1, JS2: TLInt;
  var B: TLMatrix; const ID1, ID2, JD1, JD2: TLInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure InplaceTranspose(var A: TLMatrix; const I1, I2, J1, J2: TLInt; var WORK: TLVec); {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CopyAndTranspose(const A: TLMatrix; IS1, IS2, JS1, JS2: TLInt;
  var B: TLMatrix; ID1, ID2, JD1, JD2: TLInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MatrixVectorMultiply(const A: TLMatrix; const I1, I2, J1, J2: TLInt; const Trans: Boolean;
  const X: TLVec; const IX1, IX2: TLInt; const Alpha: TLFloat;
  var Y: TLVec; const IY1, IY2: TLInt; const Beta: TLFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}

function Pythag2(X: TLFloat; Y: TLFloat): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MatrixMatrixMultiply(const A: TLMatrix; const AI1, AI2, AJ1, AJ2: TLInt; const TransA: Boolean;
  const B: TLMatrix; const BI1, BI2, BJ1, BJ2: TLInt; const TransB: Boolean;
  const Alpha: TLFloat;
  var C: TLMatrix; const CI1, CI2, CJ1, CJ2: TLInt;
  const Beta: TLFloat;
  var WORK: TLVec); {$IFDEF INLINE_ASM} inline; {$ENDIF}

{ Level 2 and Level 3 BLAS operations }
procedure ABLASSplitLength(const A: TLMatrix; N: TLInt; var N1: TLInt; var N2: TLInt);
procedure ABLASComplexSplitLength(const A: TLComplexMatrix; N: TLInt; var N1: TLInt; var N2: TLInt);
function ABLASBlockSize(const A: TLMatrix): TLInt;
function ABLASComplexBlockSize(const A: TLComplexMatrix): TLInt;
function ABLASMicroBlockSize(): TLInt;
procedure CMatrixTranspose(m: TLInt; N: TLInt; const A: TLComplexMatrix; IA: TLInt; JA: TLInt; var B: TLComplexMatrix; IB: TLInt; JB: TLInt);
procedure RMatrixTranspose(m: TLInt; N: TLInt; const A: TLMatrix; IA: TLInt; JA: TLInt; var B: TLMatrix; IB: TLInt; JB: TLInt);
procedure CMatrixCopy(m: TLInt; N: TLInt; const A: TLComplexMatrix; IA: TLInt; JA: TLInt; var B: TLComplexMatrix; IB: TLInt; JB: TLInt);
procedure RMatrixCopy(m: TLInt; N: TLInt; const A: TLMatrix; IA: TLInt; JA: TLInt; var B: TLMatrix; IB: TLInt; JB: TLInt);
procedure CMatrixRank1(m: TLInt; N: TLInt; var A: TLComplexMatrix; IA: TLInt; JA: TLInt; var U: TLComplexVec; IU: TLInt; var v: TLComplexVec; IV: TLInt);
procedure RMatrixRank1(m: TLInt; N: TLInt; var A: TLMatrix; IA: TLInt; JA: TLInt; var U: TLVec; IU: TLInt; var v: TLVec; IV: TLInt);
procedure CMatrixMV(m: TLInt; N: TLInt; var A: TLComplexMatrix; IA: TLInt; JA: TLInt; OpA: TLInt; var X: TLComplexVec; IX: TLInt; var Y: TLComplexVec; IY: TLInt);
procedure RMatrixMV(m: TLInt; N: TLInt; var A: TLMatrix; IA: TLInt; JA: TLInt; OpA: TLInt; var X: TLVec; IX: TLInt; var Y: TLVec; IY: TLInt);

procedure CMatrixRightTRSM(m: TLInt; N: TLInt;
  const A: TLComplexMatrix; I1: TLInt; J1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var X: TLComplexMatrix; I2: TLInt; J2: TLInt);

procedure CMatrixLeftTRSM(m: TLInt; N: TLInt;
  const A: TLComplexMatrix; I1: TLInt; J1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var X: TLComplexMatrix; I2: TLInt; J2: TLInt);

procedure RMatrixRightTRSM(m: TLInt; N: TLInt;
  const A: TLMatrix; I1: TLInt; J1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var X: TLMatrix; I2: TLInt; J2: TLInt);

procedure RMatrixLeftTRSM(m: TLInt; N: TLInt;
  const A: TLMatrix; I1: TLInt; J1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var X: TLMatrix; I2: TLInt; J2: TLInt);

procedure CMatrixSYRK(N: TLInt; k: TLInt; Alpha: TLFloat;
  const A: TLComplexMatrix; IA: TLInt; JA: TLInt;
  OpTypeA: TLInt; Beta: TLFloat; var C: TLComplexMatrix; IC: TLInt; JC: TLInt; IsUpper: Boolean);

procedure RMatrixSYRK(N: TLInt; k: TLInt; Alpha: TLFloat;
  const A: TLMatrix; IA: TLInt; JA: TLInt;
  OpTypeA: TLInt; Beta: TLFloat; var C: TLMatrix; IC: TLInt; JC: TLInt; IsUpper: Boolean);

procedure CMatrixGEMM(m: TLInt; N: TLInt; k: TLInt;
  Alpha: TLComplex; const A: TLComplexMatrix; IA: TLInt;
  JA: TLInt; OpTypeA: TLInt; const B: TLComplexMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt; Beta: TLComplex;
  var C: TLComplexMatrix; IC: TLInt; JC: TLInt);

procedure RMatrixGEMM(m: TLInt; N: TLInt; k: TLInt;
  Alpha: TLFloat; const A: TLMatrix; IA: TLInt;
  JA: TLInt; OpTypeA: TLInt; const B: TLMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt; Beta: TLFloat;
  var C: TLMatrix; IC: TLInt; JC: TLInt);

{ LU and Cholesky decompositions }
procedure RMatrixLU(var A: TLMatrix; m: TLInt; N: TLInt; var Pivots: TLIVec);
procedure CMatrixLU(var A: TLComplexMatrix; m: TLInt; N: TLInt; var Pivots: TLIVec);
function HPDMatrixCholesky(var A: TLComplexMatrix; N: TLInt; IsUpper: Boolean): Boolean;
function SPDMatrixCholesky(var A: TLMatrix; N: TLInt; IsUpper: Boolean): Boolean;
procedure RMatrixLUP(var A: TLMatrix; m: TLInt; N: TLInt; var Pivots: TLIVec);
procedure CMatrixLUP(var A: TLComplexMatrix; m: TLInt; N: TLInt; var Pivots: TLIVec);
procedure RMatrixPLU(var A: TLMatrix; m: TLInt; N: TLInt; var Pivots: TLIVec);
procedure CMatrixPLU(var A: TLComplexMatrix; m: TLInt; N: TLInt; var Pivots: TLIVec);

{ matrix safe }
function RMatrixScaledTRSafeSolve(const A: TLMatrix; SA: TLFloat;
  N: TLInt; var X: TLVec; IsUpper: Boolean; Trans: TLInt;
  IsUnit: Boolean; MaxGrowth: TLFloat): Boolean;

function CMatrixScaledTRSafeSolve(const A: TLComplexMatrix; SA: TLFloat;
  N: TLInt; var X: TLComplexVec; IsUpper: Boolean;
  Trans: TLInt; IsUnit: Boolean; MaxGrowth: TLFloat): Boolean;

{ * Condition number estimate support * }
function RMatrixRCond1(A: TLMatrix; N: TLInt): TLFloat;
function RMatrixRCondInf(A: TLMatrix; N: TLInt): TLFloat;
function SPDMatrixRCond(A: TLMatrix; N: TLInt; IsUpper: Boolean): TLFloat;
function RMatrixTRRCond1(const A: TLMatrix; N: TLInt; IsUpper: Boolean; IsUnit: Boolean): TLFloat;
function RMatrixTRRCondInf(const A: TLMatrix; N: TLInt; IsUpper: Boolean; IsUnit: Boolean): TLFloat;
function HPDMatrixRCond(A: TLComplexMatrix; N: TLInt; IsUpper: Boolean): TLFloat;
function CMatrixRCond1(A: TLComplexMatrix; N: TLInt): TLFloat;
function CMatrixRCondInf(A: TLComplexMatrix; N: TLInt): TLFloat;
function RMatrixLURCond1(const LUA: TLMatrix; N: TLInt): TLFloat;
function RMatrixLURCondInf(const LUA: TLMatrix; N: TLInt): TLFloat;
function SPDMatrixCholeskyRCond(const A: TLMatrix; N: TLInt; IsUpper: Boolean): TLFloat;
function HPDMatrixCholeskyRCond(const A: TLComplexMatrix; N: TLInt; IsUpper: Boolean): TLFloat;
function CMatrixLURCond1(const LUA: TLComplexMatrix; N: TLInt): TLFloat;
function CMatrixLURCondInf(const LUA: TLComplexMatrix; N: TLInt): TLFloat;
function CMatrixTRRCond1(const A: TLComplexMatrix; N: TLInt; IsUpper: Boolean; IsUnit: Boolean): TLFloat;
function CMatrixTRRCondInf(const A: TLComplexMatrix; N: TLInt; IsUpper: Boolean; IsUnit: Boolean): TLFloat;
function RCondThreshold(): TLFloat;

{ Matrix inverse }
type
  TMatInvReport = packed record
    R1: TLFloat;
    RInf: TLFloat;
  end;

procedure RMatrixLUInverse(var A: TLMatrix; const Pivots: TLIVec; N: TLInt; var Info: TLInt; var Rep: TMatInvReport);
procedure RMatrixInverse(var A: TLMatrix; N: TLInt; var Info: TLInt; var Rep: TMatInvReport);
procedure CMatrixLUInverse(var A: TLComplexMatrix; const Pivots: TLIVec; N: TLInt; var Info: TLInt; var Rep: TMatInvReport);
procedure CMatrixInverse(var A: TLComplexMatrix; N: TLInt; var Info: TLInt; var Rep: TMatInvReport);
procedure SPDMatrixCholeskyInverse(var A: TLMatrix; N: TLInt; IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
procedure SPDMatrixInverse(var A: TLMatrix; N: TLInt; IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
procedure HPDMatrixCholeskyInverse(var A: TLComplexMatrix; N: TLInt; IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
procedure HPDMatrixInverse(var A: TLComplexMatrix; N: TLInt; IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
procedure RMatrixTRInverse(var A: TLMatrix; N: TLInt; IsUpper: Boolean; IsUnit: Boolean; var Info: TLInt; var Rep: TMatInvReport);
procedure CMatrixTRInverse(var A: TLComplexMatrix; N: TLInt; IsUpper: Boolean; IsUnit: Boolean; var Info: TLInt; var Rep: TMatInvReport);

{ matrix rotations }
procedure ApplyRotationsFromTheLeft(IsForward: Boolean; M1: TLInt; M2: TLInt; N1: TLInt; N2: TLInt;
  const C: TLVec; const s: TLVec; var A: TLMatrix; var WORK: TLVec); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure ApplyRotationsFromTheRight(IsForward: Boolean; M1: TLInt; M2: TLInt; N1: TLInt; N2: TLInt;
  const C: TLVec; const s: TLVec; var A: TLMatrix; var WORK: TLVec); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure GenerateRotation(f: TLFloat; G: TLFloat; var CS: TLFloat; var SN: TLFloat; var R: TLFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}

{ Bidiagonal SVD }
function RMatrixBDSVD(var d: TLVec; E: TLVec; N: TLInt; IsUpper: Boolean; IsFractionalAccuracyRequired: Boolean;
  var U: TLMatrix; NRU: TLInt; var C: TLMatrix; NCC: TLInt; var VT: TLMatrix; NCVT: TLInt): Boolean;

function BidiagonalSVDDecomposition(var d: TLVec; E: TLVec; N: TLInt; IsUpper: Boolean; IsFractionalAccuracyRequired: Boolean;
  var U: TLMatrix; NRU: TLInt; var C: TLMatrix; NCC: TLInt; var VT: TLMatrix; NCVT: TLInt): Boolean;

{ Eigensolvers }
function SMatrixEVD(A: TLMatrix; N: TLInt; ZNeeded: TLInt; IsUpper: Boolean; var d: TLVec; var Z: TLMatrix): Boolean;

function SMatrixEVDR(A: TLMatrix; N: TLInt; ZNeeded: TLInt;
  IsUpper: Boolean; B1: TLFloat; B2: TLFloat; var m: TLInt;
  var W: TLVec; var Z: TLMatrix): Boolean;

function SMatrixEVDI(A: TLMatrix; N: TLInt; ZNeeded: TLInt;
  IsUpper: Boolean; I1: TLInt; I2: TLInt;
  var W: TLVec; var Z: TLMatrix): Boolean;

function HMatrixEVD(A: TLComplexMatrix; N: TLInt; ZNeeded: TLInt; IsUpper: Boolean;
  var d: TLVec; var Z: TLComplexMatrix): Boolean;

function HMatrixEVDR(A: TLComplexMatrix; N: TLInt;
  ZNeeded: TLInt; IsUpper: Boolean; B1: TLFloat; B2: TLFloat;
  var m: TLInt; var W: TLVec; var Z: TLComplexMatrix): Boolean;

function HMatrixEVDI(A: TLComplexMatrix; N: TLInt;
  ZNeeded: TLInt; IsUpper: Boolean; I1: TLInt;
  I2: TLInt; var W: TLVec; var Z: TLComplexMatrix): Boolean;

function SMatrixTDEVD(var d: TLVec; E: TLVec; N: TLInt; ZNeeded: TLInt; var Z: TLMatrix): Boolean;

function SMatrixTDEVDR(var d: TLVec; const E: TLVec;
  N: TLInt; ZNeeded: TLInt; A: TLFloat; B: TLFloat;
  var m: TLInt; var Z: TLMatrix): Boolean;

function SMatrixTDEVDI(var d: TLVec; const E: TLVec;
  N: TLInt; ZNeeded: TLInt; I1: TLInt;
  I2: TLInt; var Z: TLMatrix): Boolean;

function RMatrixEVD(A: TLMatrix; N: TLInt; VNeeded: TLInt;
  var WR: TLVec; var WI: TLVec; var VL: TLMatrix;
  var VR: TLMatrix): Boolean;

function InternalBisectionEigenValues(d: TLVec; E: TLVec;
  N: TLInt; IRANGE: TLInt; IORDER: TLInt;
  VL: TLFloat; VU: TLFloat; IL: TLInt; IU: TLInt;
  ABSTOL: TLFloat; var W: TLVec; var m: TLInt;
  var NSPLIT: TLInt; var IBLOCK: TLIVec;
  var ISPLIT: TLIVec; var ErrorCode: TLInt): Boolean;

procedure InternalDSTEIN(const N: TLInt; const d: TLVec;
  E: TLVec; const m: TLInt; W: TLVec;
  const IBLOCK: TLIVec; const ISPLIT: TLIVec;
  var Z: TLMatrix; var IFAIL: TLIVec; var Info: TLInt);

{ Schur decomposition }
function RMatrixSchur(var A: TLMatrix; N: TLInt; var s: TLMatrix): Boolean;
function UpperHessenbergSchurDecomposition(var H: TLMatrix; N: TLInt; var s: TLMatrix): Boolean;

{$ENDREGION 'LowLevelMatrix'}

{$REGION 'LowlevelDistribution'}

{ Normal distribution support }
function NormalDistribution(const X: TLFloat): TLFloat;
function InvNormalDistribution(const y0: TLFloat): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}

{ statistics base }
function Log1P(const X: TLFloat): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ExpM1(const X: TLFloat): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CosM1(const X: TLFloat): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Gamma support }
function Gamma(const X: TLFloat): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Natural logarithm of gamma function }
function LnGamma(const X: TLFloat; var SgnGam: TLFloat): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Incomplete gamma integral }
function IncompleteGamma(const A, X: TLFloat): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Complemented incomplete gamma integral }
function IncompleteGammaC(const A, X: TLFloat): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Inverse of complemented imcomplete gamma integral }
function InvIncompleteGammaC(const A, y0: TLFloat): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}

{ Poisson distribution }
function PoissonDistribution(k: TLInt; m: TLFloat): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Complemented Poisson distribution }
function PoissonCDistribution(k: TLInt; m: TLFloat): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Inverse Poisson distribution }
function InvPoissonDistribution(k: TLInt; Y: TLFloat): TLFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}

{ Incomplete beta integral support }
function IncompleteBeta(A, B, X: TLFloat): TLFloat;
{ Inverse of imcomplete beta integral }
function InvIncompleteBeta(const A, B, Y: TLFloat): TLFloat;

{ F distribution support }
function FDistribution(const A: TLInt; const B: TLInt; const X: TLFloat): TLFloat;
{ Complemented F distribution }
function FCDistribution(const A: TLInt; const B: TLInt; const X: TLFloat): TLFloat;
{ Inverse of complemented F distribution }
function InvFDistribution(const A: TLInt; const B: TLInt; const Y: TLFloat): TLFloat;
{ Two-sample F-test }
procedure FTest(const X: TLVec; N: TLInt; const Y: TLVec; m: TLInt; var BothTails, LeftTail, RightTail: TLFloat);

{ Binomial distribution support }
function BinomialDistribution(const k, N: TLInt; const p: TLFloat): TLFloat;
{ Complemented binomial distribution }
function BinomialCDistribution(const k, N: TLInt; const p: TLFloat): TLFloat;
{ Inverse binomial distribution }
function InvBinomialDistribution(const k, N: TLInt; const Y: TLFloat): TLFloat;
{ Sign test }
procedure OneSampleSignTest(const X: TLVec; N: TLInt; Median: TLFloat; var BothTails, LeftTail, RightTail: TLFloat);

{ Chi-square distribution support }
function ChiSquareDistribution(const v, X: TLFloat): TLFloat;
{ Complemented Chi-square distribution }
function ChiSquareCDistribution(const v, X: TLFloat): TLFloat;
{ Inverse of complemented Chi-square distribution }
function InvChiSquareDistribution(const v, Y: TLFloat): TLFloat;
{ One-sample chi-square test }
procedure OneSampleVarianceTest(const X: TLVec; N: TLInt; Variance: TLFloat; var BothTails, LeftTail, RightTail: TLFloat);

{ Student's t distribution support }
function StudentTDistribution(const k: TLInt; const t: TLFloat): TLFloat;
{ Functional inverse of Student's t distribution }
function InvStudentTDistribution(const k: TLInt; p: TLFloat): TLFloat;
{ One-sample t-test }
procedure StudentTTest1(const X: TLVec; N: TLInt; Mean: TLFloat; var BothTails, LeftTail, RightTail: TLFloat);
{ Two-sample pooled test }
procedure StudentTTest2(const X: TLVec; N: TLInt; const Y: TLVec; m: TLInt; var BothTails, LeftTail, RightTail: TLFloat);
{ Two-sample unpooled test }
procedure UnequalVarianceTTest(const X: TLVec; N: TLInt; const Y: TLVec; m: TLInt; var BothTails, LeftTail, RightTail: TLFloat);

{ Pearson and Spearman distribution support }
{ Pearson product-moment correlation coefficient }
function PearsonCorrelation(const X, Y: TLVec; const N: TLInt): TLFloat;
{ Spearman's rank correlation coefficient }
function SpearmanRankCorrelation(const X, Y: TLVec; const N: TLInt): TLFloat;
procedure SpearmanRank(var X: TLVec; N: TLInt);
{ Pearson's correlation coefficient significance test }
procedure PearsonCorrelationSignificance(const R: TLFloat; const N: TLInt; var BothTails, LeftTail, RightTail: TLFloat);
{ Spearman's rank correlation coefficient significance test }
procedure SpearmanRankCorrelationSignificance(const R: TLFloat; const N: TLInt; var BothTails, LeftTail, RightTail: TLFloat);

{ Jarque-Bera test }
procedure JarqueBeraTest(const X: TLVec; const N: TLInt; var p: TLFloat);

{ Mann-Whitney U-test }
procedure MannWhitneyUTest(const X: TLVec; N: TLInt; const Y: TLVec; m: TLInt; var BothTails, LeftTail, RightTail: TLFloat);

{ Wilcoxon signed-rank test }
procedure WilcoxonSignedRankTest(const X: TLVec; N: TLInt; E: TLFloat; var BothTails, LeftTail, RightTail: TLFloat);
{$ENDREGION 'LowlevelDistribution'}

{$REGION 'LowLevelGauss'}
{
  Computation of nodes and weights for a Gauss quadrature formula

  The algorithm generates the N-point Gauss quadrature formula with weight
  function given by coefficients alpha and beta of a recurrence relation
  which generates a system of orthogonal polynomials:

  P-1(x)   =  0
  P0(x)    =  1
  Pn+1(x)  =  (x-alpha(n))*Pn(x)  -  beta(n)*Pn-1(x)

  and zeroth moment Mu0

  Mu0 = integral(W(x)dx,a,b)
}
procedure GaussQuadratureGenerateRec(const Alpha, Beta: TLVec; const Mu0: TLFloat; N: TLInt; var Info: TLInt; var X: TLVec; var W: TLVec); {$IFDEF INLINE_ASM} inline; {$ENDIF}
{
  Computation of nodes and weights for a Gauss-Lobatto quadrature formula

  The algorithm generates the N-point Gauss-Lobatto quadrature formula with
  weight function given by coefficients alpha and beta of a recurrence which
  generates a system of orthogonal polynomials.

  P-1(x)   =  0
  P0(x)    =  1
  Pn+1(x)  =  (x-alpha(n))*Pn(x)  -  beta(n)*Pn-1(x)

  and zeroth moment Mu0

  Mu0 = integral(W(x)dx,a,b)
}
procedure GaussQuadratureGenerateGaussLobattoRec(const Alpha, Beta: TLVec; const Mu0, A, B: TLFloat; N: TLInt; var Info: TLInt; var X: TLVec; var W: TLVec); {$IFDEF INLINE_ASM} inline; {$ENDIF}
{
  Computation of nodes and weights for a Gauss-Radau quadrature formula

  The algorithm generates the N-point Gauss-Radau quadrature formula with
  weight function given by the coefficients alpha and beta of a recurrence
  which generates a system of orthogonal polynomials.

  P-1(x)   =  0
  P0(x)    =  1
  Pn+1(x)  =  (x-alpha(n))*Pn(x)  -  beta(n)*Pn-1(x)

  and zeroth moment Mu0

  Mu0 = integral(W(x)dx,a,b)
}
procedure GaussQuadratureGenerateGaussRadauRec(const Alpha, Beta: TLVec; const Mu0, A: TLFloat; N: TLInt; var Info: TLInt; var X: TLVec; var W: TLVec); {$IFDEF INLINE_ASM} inline; {$ENDIF}

{ Returns nodes/weights for Gauss-Legendre quadrature on [-1,1] with N nodes }
procedure GaussQuadratureGenerateGaussLegendre(const N: TLInt; var Info: TLInt; var X: TLVec; var W: TLVec); {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Returns nodes/weights for Gauss-Jacobi quadrature on [-1,1] with weight function W(x)=Power(1-x,Alpha)*Power(1+x,Beta) }
procedure GaussQuadratureGenerateGaussJacobi(const N: TLInt; const Alpha, Beta: TLFloat; var Info: TLInt; var X: TLVec; var W: TLVec); {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Returns nodes/weights for Gauss-Laguerre quadrature on (0,+inf) with weight function W(x)=Power(x,Alpha)*Exp(-x) }
procedure GaussQuadratureGenerateGaussLaguerre(const N: TLInt; const Alpha: TLFloat; var Info: TLInt; var X: TLVec; var W: TLVec); {$IFDEF INLINE_ASM} inline; {$ENDIF}
{ Returns nodes/weights for Gauss-Hermite quadrature on (-inf,+inf) with weight function W(x)=Exp(-x*x) }
procedure GaussQuadratureGenerateGaussHermite(const N: TLInt; var Info: TLInt; var X: TLVec; var W: TLVec); {$IFDEF INLINE_ASM} inline; {$ENDIF}

{
  Computation of nodes and weights of a Gauss-Kronrod quadrature formula

  The algorithm generates the N-point Gauss-Kronrod quadrature formula  with
  weight function given by coefficients alpha and beta of a recurrence
  relation which generates a system of orthogonal polynomials:

  P-1(x)   =  0
  P0(x)    =  1
  Pn+1(x)  =  (x-alpha(n))*Pn(x)  -  beta(n)*Pn-1(x)

  and zero moment Mu0

  Mu0 = integral(W(x)dx,a,b)
}
procedure GaussKronrodQuadratureGenerateRec(const Alpha, Beta: TLVec; const Mu0: TLFloat; N: TLInt; var Info: TLInt; var X, WKronrod, WGauss: TLVec); {$IFDEF INLINE_ASM} inline; {$ENDIF}
{
  Returns Gauss and Gauss-Kronrod nodes/weights for Gauss-Legendre quadrature with N points.
  GKQLegendreCalc (calculation) or GKQLegendreTbl (precomputed table) is used depending on machine precision and number of nodes.
}
procedure GaussKronrodQuadratureGenerateGaussLegendre(const N: TLInt; var Info: TLInt; var X, WKronrod, WGauss: TLVec); {$IFDEF INLINE_ASM} inline; {$ENDIF}
{
  Returns Gauss and Gauss-Kronrod nodes/weights for Gauss-Jacobi quadrature on [-1,1] with weight function
  W(x)=Power(1-x,Alpha)*Power(1+x,Beta).
}
procedure GaussKronrodQuadratureGenerateGaussJacobi(const N: TLInt; const Alpha, Beta: TLFloat; var Info: TLInt; var X, WKronrod, WGauss: TLVec); {$IFDEF INLINE_ASM} inline; {$ENDIF}
{
  Returns Gauss and Gauss-Kronrod nodes for quadrature with N points.
  Reduction to tridiagonal eigenproblem is used.
}
procedure GaussKronrodQuadratureLegendreCalc(const N: TLInt; var Info: TLInt; var X, WKronrod, WGauss: TLVec); {$IFDEF INLINE_ASM} inline; {$ENDIF}
{
  Returns Gauss and Gauss-Kronrod nodes for quadrature with N  points  using pre-calculated table. Nodes/weights were computed with accuracy up to 1.0E-32.
  In standard TLFloat  precision accuracy reduces to something about 2.0E-16 (depending  on your compiler's handling of long floating point constants).
}
procedure GaussKronrodQuadratureLegendreTbl(const N: TLInt; var X, WKronrod, WGauss: TLVec; var Eps: TLFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
{$ENDREGION 'LowLevelGauss'}

procedure LearnTest;

const
  // IEEE floating
  MachineEpsilon = 5.0E-16;
  MaxRealNumber  = 1.0E300;
  MinRealNumber  = 1.0E-300;

implementation

uses KM, Math,
{$IFDEF FPC}
  mtprocs,
{$ELSE}
  Threading,
{$ENDIF FPC}
  SyncObjs, PyramidSpace, FastHistogramSpace, DoStatusIO;

const
  SYSTEM_HOGCELLSIZE     = 10;
  SYSTEM_HOGSAMPLERSIZE  = 12;
  SYSTEM_HOG_FEATURESIZE = 12 * 12 * (4 + 9 + 18);

var
  System_HOGTable: THOGTable;

{$REGION 'Include'}
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
{$INCLUDE learn_gaussintegral.inc}
{$INCLUDE learn_extAPI.inc}
{$INCLUDE learn_th.inc}
{$INCLUDE learn_class.inc}
{$INCLUDE learn_test.inc}
{$ENDREGION 'Include'}

initialization

System_HOGTable := THOGTable.Create(9, 18, SYSTEM_HOGCELLSIZE);

finalization

DisposeObject(System_HOGTable);

end.
