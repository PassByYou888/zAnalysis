{ ****************************************************************************** }
{ * h264BitStream.pas        by qq600585                                       * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

unit h264BitStream;

{$I zDefine.inc}
{$POINTERMATH ON}

interface

uses h264Stdint;

type
  TBitStreamWriter = class
  private
    buffer: uint32_p;
    cur: uint32_p;
    mask: uint32_t;
    closed: boolean;
    function GetBitSize: uint32_t;
    function GetByteSize: uint32_t;
    function GetDataStart: uint8_p;
  public
    property BitSize: uint32_t read GetBitSize;
    property ByteSize: uint32_t read GetByteSize;
    property DataStart: uint8_p read GetDataStart;
    constructor Create(const memory_buffer: uint8_p);
    destructor Destroy; override;
    procedure Close;
    function IsByteAligned: boolean;
    procedure ByteAlign;
    procedure Write(const bit: int32_t); overload;
    procedure Write(bits, bit_count: uint32_t); overload;
  end;

implementation

function bswap(n: uint32_t): uint32_t; inline;
begin
  result := (n shr 24) or (n shl 24) or ((n shr 8) and $FF00) or ((n shl 8) and $FF0000);
end;

{ TBitstreamWriter }

function TBitStreamWriter.GetBitSize: uint32_t;
begin
  result := 32 * (cur - buffer) + (32 - int32_t(mask));
end;

function TBitStreamWriter.GetByteSize: uint32_t;
begin
  result := (cur - buffer) * 4;
  inc(result, (32 - mask + 7) div 8); // + buffer
end;

function TBitStreamWriter.GetDataStart: uint8_p;
begin
  result := uint8_p(buffer);
end;

constructor TBitStreamWriter.Create(const memory_buffer: uint8_p);
begin
  inherited Create;
  buffer := uint32_p(memory_buffer);
  cur := buffer;
  cur^ := 0;
  mask := 32;
end;

destructor TBitStreamWriter.Destroy;
begin
  if not closed then
      Close;
  inherited Destroy;
end;

procedure TBitStreamWriter.Close;
begin
  if not closed then
    begin
      if mask < 32 then
          cur^ := bswap(cur^);
      closed := true;
    end;
end;

function TBitStreamWriter.IsByteAligned: boolean;
begin
  result := mask mod 8 = 0;
end;

procedure TBitStreamWriter.ByteAlign;
begin
  while not IsByteAligned do
      write(0);
end;

procedure TBitStreamWriter.Write(const bit: int32_t);
begin
  dec(mask);
  cur^ := cur^ or uint32_t((bit and 1) shl mask);

  if mask = 0 then
    begin
      cur^ := bswap(cur^);
      inc(cur);
      cur^ := 0;
      mask := 32;
    end;
end;

procedure TBitStreamWriter.Write(bits, bit_count: uint32_t);
var
  bits_left: uint32_t;
begin
  bits := bits and ($FFFFFFFF shr (32 - bit_count)); // safety check
  if mask > bit_count then
    begin
      dec(mask, bit_count);
      cur^ := cur^ or (bits shl mask);
    end
  else
    begin
      bits_left := bit_count - mask;
      mask := 32 - bits_left;
      cur^ := cur^ or (bits shr bits_left);
      cur^ := bswap(cur^);
      inc(cur);
      cur^ := 0;
      if bits_left > 0 then
          cur^ := bits shl mask;
    end;
end;

end.
