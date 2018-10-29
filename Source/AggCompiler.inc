{$IFDEF FPC}
  {$MODE DELPHI}
  {$MODESWITCH AdvancedRecords}
  {$STACKFRAMES OFF}
  {$NOTES OFF}
  {$IFDEF FPC_BIG_ENDIAN}
    {$DEFINE BIG_ENDIAN}
  {$ENDIF}

  {$UNDEF FirstCharInZero}

  {$UNDEF Delphi}
  {$UNDEF FastMD5}
  {$DEFINE release}
  {$UNDEF debug}
  {$DEFINE INLINE_ASM}
  {$R-} { range check }
{$ELSE}
  {$IFDEF release}
    {$R-} { range check }
  {$ELSE}
    {$R-} { range check }
  {$ENDIF}

  {$DEFINE Delphi}
{$ENDIF}

{$WARNINGS OFF}
{$HINTS OFF}
{$H+} { long string }
{$A+} { Word Align Data }
{$O+} { optimization }
{$Q-} { Overflow checking }
{$B-} { Complete boolean evaluation }
{$I-} { Input output checking }
{$J+} { Writeable typed constants }

{$IFDEF release}
  {$DEFINE INLINE_ASM}
{$ELSE}
  {$UNDEF INLINE_ASM}
{$ENDIF}

{$UNDEF CPU64}

{$IFDEF CPU64BITS}
  {$DEFINE CPU64}
{$ELSE}
  {$IFDEF CPUX64}
    {$DEFINE CPU64}
  {$ENDIF}
{$ENDIF}

{$IFNDEF CPU64}
  {$DEFINE CPU32}
{$ENDIF}

{$IFDEF BIG_ENDIAN}
  {$MESSAGE FATAL 'Big-endian system not supported'}
{$ENDIF}

{$IFOPT R+}
  {$DEFINE RangeCheck}
{$ENDIF}

{$IFOPT Q+}
  {$DEFINE OverflowCheck}
{$ENDIF}

