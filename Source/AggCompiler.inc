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

  {$R-} { range check }

  {$UNDEF Delphi}
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
{$O-} { optimization }
{$Q-} { Overflow checking }
{$B-} { Complete boolean evaluation }
{$I+} { Input output checking }

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

