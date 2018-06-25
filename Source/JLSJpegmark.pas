{ ****************************************************************************** }
{ * JPEG-LS Codec https://github.com/zekiguven/pascal_jls                      * }
{ * fixed by QQ 600585@qq.com                                                  * }
{ ****************************************************************************** }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
{
  JPEG-LS Codec
  This code is based on http://www.stat.columbia.edu/~jakulin/jpeg-ls/mirror.htm
  Converted from C to Pascal. 2017

  https://github.com/zekiguven/pascal_jls

  author : Zeki Guven
}
unit JLSJpegmark;

{$I zDefine.inc}

interface

uses
  JLSGlobal,
  JLSBitIO,
  CoreClasses,
  MemoryStream64;

{ Marker identifiers }
const
  // publish marker
  JPEGLS_MARKER_SOI = $FFD8; { start of image } // modify by 600585@qq.com
  JPEGLS_MARKER_EOI = $FFD9; { end of image }   // modify by 600585@qq.com
  // custom marker
  JPEGLS_MARKER_SOI2 = $FF8D; { start of image } // modify by 600585@qq.com
  JPEGLS_MARKER_EOI2 = $FF9D; { end of image }   // modify by 600585@qq.com
  // publish marker
  JPEGLS_MARKER_SOS  = $FFDA; { Start of scan }
  JPEGLS_MARKER_DNL  = $FFDC; { Define number of lines }
  JPEGLS_MARKER_DRI  = $FFDD; { Define restart interval }
  JPEGLS_MARKER_RSTm = $FFD0; { Restart marker (FFD0-FFD7) }
  JPEGLS_MARKER_COM  = $FFFE; { Comment }

  { JPEG-LS specific }
  SOF_LS        = $FFF7; { Start of JPEG-LS regular frame }
  LSE           = $FFF8; { JPEG-LS extension marker }
  LSE_PARAMS    = 1;     { Marker type within LSE - parameters }
  LSE_MAPTABLE  = 2;     { Marker type within LSE - map tables }
  LSE_XMAPTABLE = 3;     { Marker type within LSE - map table continuation }
  LSE_XY        = 4;     { Marker type within LSE - image dimensions }

type
  TJLSJpegMark = class
  private
    FBitIO: TJLSBitIO;
    FImageInfo: PImageInfo;
  public
    constructor Create(ABitIO: TJLSBitIO; AImageInfo: PImageInfo);

    { Functions to write markers }
    function write_n_bytes(outstrm: TCoreClassStream; value, n: int): int;
    function write_2_bytes(outstrm: TCoreClassStream; value: int): int;
    function write_marker(outstrm: TCoreClassStream; marker: int): int;
    function write_jpegls_frame(outstrm: TCoreClassStream; jp: pjpeg_ls_header): int;
    function write_jpegls_scan(outstrm: TCoreClassStream; jp: pjpeg_ls_header): int;
    function write_jpegls_extmarker(outstrm: TCoreClassStream; jp: pjpeg_ls_header; IDtype: int): int;
    function write_jpegls_restartmarker(outstrm: TCoreClassStream; jp: pjpeg_ls_header): int;

    { Functions to read markers }
    function read_n_bytes(instrm: TCoreClassStream; n: int): uint;
    function read_marker(instrm: TCoreClassStream; mkp: pint): int;
    function seek_marker(instrm: TCoreClassStream; mkp: pint): int;
    function read_jpegls_frame(instrm: TCoreClassStream; jp: pjpeg_ls_header): int;
    function read_jpegls_scan(instrm: TCoreClassStream; jp: pjpeg_ls_header): int;
    function read_jpegls_extmarker(instrm: TCoreClassStream; jp: pjpeg_ls_header): int;
    function read_jpegls_restartmarker(instrm: TCoreClassStream; jp: pjpeg_ls_header): int;
  end;

implementation


procedure check_range(param: int; name: string; low, high: int);
begin
  if (param < low) or (param > high) then
    begin
      RaiseInfo('Allowed range for %s is [%d..%d]: got %d', [name, low, high, param]);
      exit;
    end;
end;

{ *
  *
  *   Marker output functions
  *
  * }

function TJLSJpegMark.write_n_bytes(outstrm: TCoreClassStream; value, n: int): int;
var
  l: int;
  i: byte;
begin

  if (n > 4) then
    begin
      RaiseInfo('write_n_bytes: Only 32 bits variables supported.');
      Result := 10;
      exit;
    end;

  for l := n - 1 downto 0 do
    begin
      i := shr_c(value, 8 * l) and $000000FF;
      outstrm.Write(i, 1 { sizeof(i) } );
    end;
  Result := n;
end;

function TJLSJpegMark.write_2_bytes(outstrm: TCoreClassStream; value: int): int;
begin
  Result := write_n_bytes(outstrm, value, 2);
end;

function TJLSJpegMark.write_marker(outstrm: TCoreClassStream; marker: int): int;
{ Write a two-byte marker (just the marker identifier) }
begin
  write_n_bytes(outstrm, marker, 2);
  Result := 2;
end;

function TJLSJpegMark.write_jpegls_frame(outstrm: TCoreClassStream; jp: pjpeg_ls_header): int;
var
  i, marker_len,
    bpp, ct: int;
  sx, sy: int;
begin
  ct := 0;

  ct := ct + write_marker(outstrm, SOF_LS); { write JPEG-LS frame marker }

  check_range(jp^.comp, 'frame components', 1, 255);
  marker_len := 8 + 3 * jp^.comp;

  ct := ct + write_n_bytes(outstrm, marker_len, 2); { write marker length }
  bpp := 1;
  while ((1 shl bpp) < jp^.alp) do
      inc(bpp);

  ct := ct + write_n_bytes(outstrm, bpp, 1); { write bits/sample }

  { current implementation only supports up to 64K samples in
    either direction. Also, they must be specified in the frame header }
  check_range(jp^.rows, 'rows', 1, 65535);
  check_range(jp^.columns, 'columns', 1, 65535);

  ct := ct + write_n_bytes(outstrm, jp^.rows, 2);    { write number of rows }
  ct := ct + write_n_bytes(outstrm, jp^.columns, 2); { write number of cols }

  ct := ct + write_n_bytes(outstrm, jp^.comp, 1);

  { now write a triplet of bytes per component }
  for i := 0 to pred(jp^.comp) do
    begin
      sx := jp^.samplingx[i];
      sy := jp^.samplingy[i];

      check_range(sx, 'sampling(x)', 1, 4);
      check_range(sy, 'sampling(y)', 1, 4);
      ct := ct + write_n_bytes(outstrm, jp^.comp_ids[i], 1);  { component identifier }
      ct := ct + write_n_bytes(outstrm, (sx shl 4) or sy, 1); { sampling rates }
      ct := ct + write_n_bytes(outstrm, 0, 1);                { Tq unused }
    end;

  Result := ct;
end;

function TJLSJpegMark.write_jpegls_scan(outstrm: TCoreClassStream; jp: pjpeg_ls_header): int;
var
  i, marker_len, ct: int;

begin
  ct := 0;

  ct := ct + write_marker(outstrm, JPEGLS_MARKER_SOS); { write JPEG-LS scan marker }

  check_range(jp^.comp, 'scan components', 1, 4);

  if (jp^.comp = 1) and (jp^.color_mode <> PLANE_INT) then
    begin
      RaiseInfo('Interleave for 1 component must be PLANE_INT: got %d', [jp^.color_mode]);
      Result := 10;
      exit;
    end;

  if (jp^.comp > 1) and (jp^.color_mode = 0) then
    begin
      RaiseInfo('Interleave for multi-component scan must be nonzero: got %d', [jp^.color_mode]);
      Result := 10;
      exit;
    end;

  marker_len := 6 + 2 * jp^.comp;

  ct := ct + write_n_bytes(outstrm, marker_len, 2); { write marker length }
  ct := ct + write_n_bytes(outstrm, jp^.comp, 1);   { # of components for the scan }

  { write 2 bytes per component }
  for i := 0 to pred(jp^.comp) do
    begin
      ct := ct + write_n_bytes(outstrm, jp^.comp_ids[i], 1); { component identifier }
      ct := ct + write_n_bytes(outstrm, 0, 1);               { no tables in this implementation }
    end;

  check_range(jp^._near, '_near', 0, 255);
  ct := ct + write_n_bytes(outstrm, jp^._near, 1);

  check_range(jp^.color_mode, 'INTERLEAVE', 0, 2);
  ct := ct + write_n_bytes(outstrm, jp^.color_mode, 1);

  check_range(jp^.shift, 'SHIFT', 0, 15);
  ct := ct + write_n_bytes(outstrm, jp^.shift, 1);

  Result := ct;
end;

function TJLSJpegMark.write_jpegls_extmarker(outstrm: TCoreClassStream; jp: pjpeg_ls_header; IDtype: int): int;
var
  marker_len, ct: int;
  TID,          { Table ID }
  Wt,           { Width of table entries }
  MAXTAB,       { Maximum index of table }
  length: uint; { Marker length }
  i: int;

begin
  ct := 0;

  { For Type 1 - non default parameters }
  if (IDtype = LSE_PARAMS) then
    begin
      ct := ct + write_marker(outstrm, LSE); { write JPEG-LS extended marker id }

      ct := ct + write_n_bytes(outstrm, 13, 2);          { marker length }
      ct := ct + write_n_bytes(outstrm, LSE_PARAMS, 1);  { ext marker id }
      ct := ct + write_n_bytes(outstrm, jp^.alp - 1, 2); { MAXVAL }
      ct := ct + write_n_bytes(outstrm, jp^.T1, 2);
      ct := ct + write_n_bytes(outstrm, jp^.T2, 2);
      ct := ct + write_n_bytes(outstrm, jp^.T3, 2);
      ct := ct + write_n_bytes(outstrm, jp^.RES, 2);

      Result := ct;
    end;

  { For Type 2 - Mapping Table }
  if (IDtype = LSE_MAPTABLE) then
    begin

    end
  else
    begin
      // fprintf(stderr, "LSE Parameter %i not defined in this implementation.\n",IDtype);
      // exit(1);
    end;

end;

{ Writes the DRI header to the JLS file }
function TJLSJpegMark.write_jpegls_restartmarker(outstrm: TCoreClassStream; jp: pjpeg_ls_header): int;
var
  ct: int;
  Ri: int;     { the restart interval (# of MCU's between markers) }
  length: int; { the length of the DRI header }
begin
  ct := 0;
  Ri := jp^.restart_interval;

  if (Ri <= 65535) then
      length := 4
  else
      length := 6;

  ct := ct + write_marker(outstrm, JPEGLS_MARKER_DRI);
  ct := ct + write_n_bytes(outstrm, length, 2);
  ct := ct + write_n_bytes(outstrm, Ri, 2);

  Result := ct;
end;

{ *
  *
  *   Marker input functions
  *
  * }

function TJLSJpegMark.seek_marker(instrm: TCoreClassStream; mkp: pint): int;
{ Seeks a marker in the input stream. Returns the marker head, or EOF }
var
  c, c2, ct: int;
begin
  ct := 0;
  c := FBitIO.mygetc;
  while (c <> BUF_EOF) do
    begin
      inc(ct);
      if (c = $FF) then
        begin
          c2 := FBitIO.mygetc;
          if (c2 = BUF_EOF) then
            begin
              Result := BUF_EOF;
              exit;
            end;

          inc(ct);

          if IsTrue(c2 and $80) then
            begin
              mkp^ := (c shl 8) or c2;
              Result := ct;
              exit;
            end;
        end;

      c := FBitIO.mygetc;
    end;
  Result := BUF_EOF;
end;

function TJLSJpegMark.read_n_bytes(instrm: TCoreClassStream; n: int): uint;
{ reads n bytes (0 <= n <= 4) from the input stream }
var
  m: uint;
  i: int;
begin
  m := 0;
  for i := 0 to pred(n) do
      m := (m shl 8) or FBitIO.mygetc;

  Result := m;
end;

function TJLSJpegMark.read_marker(instrm: TCoreClassStream; mkp: pint): int;
{ reads a marker from the next two bytes in the input stream }
var
  m, ct: uint;
begin
  ct := 0;

  m := read_n_bytes(instrm, 2);
  if ((m and $FF00) <> $FF00) then
    begin
      RaiseInfo('read_marker: Expected marker, got %04x\n', [m]);
      Result := 10;
      exit;
    end;
  mkp^ := m;
  Result := 2;
end;

function TJLSJpegMark.read_jpegls_frame(instrm: TCoreClassStream; jp: pjpeg_ls_header): int;
{ reads the JPEG-LS frame marker (not including marker head) }
var
  i,
    marker_len,
    bpp,
    tq,
    comp,
    ct: int;
  sx, sy, cid: int;

begin
  ct := 0;

  { Read Marker Length }
  marker_len := read_n_bytes(instrm, 2);
  inc(ct, 2);

  { Read the bits per pixel }
  bpp := read_n_bytes(instrm, 1);
  inc(ct);

  check_range(bpp, 'bpp', 2, 16);
  jp^.alp := 1 shl bpp;

  { Read the rows and columns }
  jp^.rows := read_n_bytes(instrm, 2);
  inc(ct, 2);
  jp^.columns := read_n_bytes(instrm, 2);
  inc(ct, 2);

  { Read component information }
  comp := read_n_bytes(instrm, 1);
  inc(ct);
  check_range(comp, 'COMP', 1, 255);
  jp^.comp := comp;

  for i := 0 to pred(comp) do
    begin

      cid := read_n_bytes(instrm, 1);
      inc(ct);
      sx := read_n_bytes(instrm, 1);
      inc(ct);
      tq := read_n_bytes(instrm, 1);
      inc(ct);
      check_range(tq, 'Tq', 0, 0);
      sy := sx and $0F;
      sx := shr_c(sx, 4);
      check_range(sx, 'sampling(x)', 1, 4);
      check_range(sy, 'sampling(y)', 1, 4);
      jp^.samplingx[i] := sx;
      jp^.samplingy[i] := sy;
      jp^.comp_ids[i] := cid;
    end;

  { Check for errors }
  if (marker_len <> 8 + 3 * comp) then
    begin
      Result := 10;
      RaiseInfo('read_jpegls_frame: inconsistent marker length: expected %d, got %d', [marker_len, 8 + 3 * comp]);
      exit;
    end;

  Result := ct;
end;

{ reads the JPEG-LS scan marker (not including marker head) }
function TJLSJpegMark.read_jpegls_scan(instrm: TCoreClassStream; jp: pjpeg_ls_header): int;
var
  i, marker_len, comp, ct: int;
  cid, tm: int;

begin
  ct := 0;

  marker_len := read_n_bytes(instrm, 2);
  inc(ct, 2);

  comp := read_n_bytes(instrm, 1);
  inc(ct, 1);
  check_range(comp, 'scan components', 1, 4);

  jp^.comp := comp;

  { read 2 bytes per component }
  for i := 0 to pred(comp) do
    begin

      cid := read_n_bytes(instrm, 1); { component identifier }
      inc(ct);
      tm := read_n_bytes(instrm, 1); { table identifier }
      inc(ct);

      if (IsTrue(tm)) then
        begin
          Result := 10;
          RaiseInfo('read_jpegls_scan: found nonzero table identifier, not supported');
          exit;
        end;

      jp^.comp_ids[i] := cid;
    end;

  jp^._near := read_n_bytes(instrm, 1);
  inc(ct);
  check_range(jp^._near, '_near', 0, 255);

  jp^.color_mode := read_n_bytes(instrm, 1);
  inc(ct);
  check_range(jp^.color_mode, 'INTERLEAVE', 0, 2);

  if (jp^.comp = 1) and (jp^.color_mode <> 0) then
    begin
      {
        fprintf(stderr,"Interleave for 1 component must be 0: got %d\n",
        jp->color_mode);
      }

      { ignore interleave value, set to 0 }
      jp^.color_mode := 0;
    end;

  if (jp^.comp > 1) and (jp^.color_mode = 0) then
    begin
      Result := 10;
      RaiseInfo('Interleave for multi-component scan must be nonzero: got %d', [jp^.color_mode]);
      exit;
    end;

  jp^.shift := read_n_bytes(instrm, 1);
  inc(ct);
  check_range(jp^.shift, 'SHIFT', 0, 15);

  if (marker_len <> 6 + 2 * comp) then
    begin
      Result := 10;
      RaiseInfo('read_jpegls_scan: inconsistent marker length: expected %d, got %d', [marker_len, 6 + 2 * comp]);
    end;
  Result := ct;
end;

{ reads the JPEG-LS extension marker (not including marker head) }
{ Supports non-default type (1) and mapping table type (2) }
constructor TJLSJpegMark.Create(ABitIO: TJLSBitIO; AImageInfo: PImageInfo);
begin
  FBitIO := ABitIO;
  FImageInfo := AImageInfo;
end;

function TJLSJpegMark.read_jpegls_extmarker(instrm: TCoreClassStream; jp: pjpeg_ls_header): int;
var
  marker_len, { marker length }
  maxval,     { max value }
  T1, T2, T3, { thresholds }
  ct,
    IDtype, { LSE type }
  TID,      { table ID }
  Wt,       { width of each table entry }
  MAXTAB,   { maximum table index }
  i: int;

begin
  ct := 0;

  { Read marker length }
  marker_len := read_n_bytes(instrm, 2); { marker length }
  inc(ct, 2);

  { Read id type }
  IDtype := read_n_bytes(instrm, 1);
  inc(ct, 1);

  { For Type 1 - non default parameters }
  if (IDtype = LSE_PARAMS) then
    begin
      if (marker_len <> 13) then
        begin
          RaiseInfo('read_jpegls_extmarker: bad marker length %d', [marker_len]);
          Result := 10;
          exit;
        end;

      { read maxval }
      maxval := read_n_bytes(instrm, 2);
      inc(ct, 2);
      jp^.alp := maxval + 1;

      { read thresholds and reset }
      jp^.T1 := read_n_bytes(instrm, 2);
      inc(ct, 2);
      jp^.T2 := read_n_bytes(instrm, 2);
      jp^.T3 := read_n_bytes(instrm, 2);
      jp^.RES := read_n_bytes(instrm, 2);
      inc(ct, 6);

      Result := ct;
      exit;
    end;

  { For Type 2 - mapping table }
  if (IDtype = LSE_MAPTABLE) then
    begin

      { Indicate table used }
      jp^.need_table := 1;

      { Read table ID }
      TID := read_n_bytes(instrm, 1);
      jp^.TID := TID;
      inc(ct, 1);

      { Read width of table entry }
      Wt := read_n_bytes(instrm, 1);
      jp^.Wt := Wt;
      if (Wt <= 0) or (Wt > 3) then
        begin
          RaiseInfo('Width of mapping table entries must be either 1,2 or 3 in this implementation. Sorry!');
          Result := 0;
          exit;
        end;
      inc(ct, 1);

      { Calculate value of MAXTAB }
      MAXTAB := ((marker_len - 5) div Wt) - 1;
      jp^.MAXTAB := MAXTAB;

      { Get table entries }
      jp^.TABLE^[TID] := safecalloc((MAXTAB + 1) * sizeof(int), 1);
      for i := 0 to MAXTAB do
        begin
          PWordArray(jp^.TABLE^[TID])^[i] := read_n_bytes(instrm, Wt);
        end;
      inc(ct, (MAXTAB + 1) * Wt);

      Result := ct;
      exit;
    end;

  { Non supported types }
  Result := 0;
  RaiseInfo('LSE marker type %i not supported in this implementation', [IDtype]);
end;

{ Read DRI restart marker }
function TJLSJpegMark.read_jpegls_restartmarker(instrm: TCoreClassStream; jp: pjpeg_ls_header): int;
var
  ct: int;
  marker_len: int; { the marker length }
  Ri: int;         { the restart interval }

begin
  ct := 0;

  { Read marker length }
  marker_len := read_n_bytes(instrm, 2);
  inc(ct, 2);

  { Read the restart interval }
  Ri := read_n_bytes(instrm, marker_len - 2);
  inc(ct, (marker_len - 2));

  jp^.restart_interval := Ri;

  Result := ct;
end;

end.
