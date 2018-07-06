{$IFDEF FPC}
  {$MODE DELPHI}
  {$MODESWITCH AdvancedRecords}
  {$STACKFRAMES ON}
  {$NOTES OFF}
  {$ASMMODE intel}
  {$IFDEF FPC_BIG_ENDIAN}
    {$DEFINE BIG_ENDIAN}
  {$ENDIF}

  {$UNDEF FirstCharInZero}

  {$UNDEF Delphi}
  {$DEFINE release}
  {$DEFINE INLINE_ASM}
  {$UNDEF Delphi}
{$ELSE}
  {$DEFINE Delphi}

  {$IFDEF release}
    {$DEFINE INLINE_ASM}
  {$ELSE}
    {$UNDEF INLINE_ASM}
  {$ENDIF}
{$ENDIF}

{$WARNINGS OFF}
{$HINTS OFF}
{$H+} { long string }
{$A+} { Word Align Data }
{$O-} { optimization }
{$Q-} { Overflow checking }
{$B-} { Complete boolean evaluation }
{$I+} { Input output checking }
{$R-} { range check }

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

