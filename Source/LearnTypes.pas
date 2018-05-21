unit LearnTypes;

interface

uses PascalStrings, KDTree, KM;

type
  TLFloat  = TKDTree_VecType;
  PLFloat  = PKDTree_VecType;
  TLVec    = TKDTree_Vec;
  PLVec    = PKDTree_Vec;
  TLMatrix = TKDTree_DynamicVecBuffer;
  PLMatrix = PKDTree_DynamicVecBuffer;

  TLInt     = TKMInt;
  PLInt     = PKMInt;
  TLIVec    = TKMIntegerArray;
  PLIVec    = PKMIntegerArray;
  TLIMatrix = packed array of TLIVec;
  PLIMatrix = ^TLIMatrix;

  TLBVec    = packed array of Boolean;
  PLBVec    = ^TLBVec;
  TLBMatrix = packed array of TLBVec;
  PLBMatrix = ^TLBMatrix;

  TLComplex = packed record
    X, Y: TLFloat;
  end;

  TLComplexVec    = packed array of TLComplex;
  TLComplexMatrix = packed array of TLComplexVec;

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

const
  CLearnString: array [TLearnType] of SystemString = (
    'k-dimensional tree',
    'k-means++ clusterization',
    'Random forest',
    'Logistic regression',
    'Levenberg-Marquardt',
    'Levenberg-Marquardt with Parallel',
    'L-BFGS',
    'L-BFGS with parallel',
    'L-BFGS with parallel and optimization',
    'fast Monte Carlo',
    'Levenberg-Marquardt Ensemble',
    'Levenberg-Marquardt Ensemble with parallel',
    'L-BFGS Ensemble',
    'L-BFGS Ensemble with parallel'
    );

implementation

end.
