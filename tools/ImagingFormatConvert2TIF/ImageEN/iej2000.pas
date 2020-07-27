(*
Copyright (c) 1998-2014 by Carlotta Calandra. All rights reserved.
Copyright (c) 2011-2014 by Xequte software.

This software comes without express or implied warranty.
In no case shall the author be liable for any damage or unwanted behavior of any
computer hardware and/or software.

Author grants you the right to include the component
in your application, whether COMMERCIAL, SHAREWARE, or FREEWARE.

ImageEn, IEvolution and ImageEn ActiveX may not be included in any
commercial, shareware or freeware libraries or components.

www.ImageEn.com
*)

(*
File version 1010
*)

unit iej2000;

{$R-}
{$Q-}


{$I ie.inc}


{$ifdef IEINCLUDEJPEG2000}




interface


uses Windows, Graphics, Classes, SysUtils, ImageEnIO, hyiedefs, hyieutils;


{$ifndef IEUSEDLLJPEG2000LIB}

var
  __turboFloat: LongBool = False;

{$endif}



function J2KTryStreamJP2(Stream: TStream): boolean;
function J2KTryStreamJ2K(Stream: TStream): boolean;
procedure J2KReadStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var xProgress: TProgressRec; Preview: boolean);
procedure J2KWriteStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var xProgress: TProgressRec; format: integer);

procedure IEFinalize_iej2000;

type
  EIEJPEG2000Exception = class(Exception);


implementation

uses ieview, imageenproc, ievision, iesettings;


const
  IEJAS_IMAGE_CS_UNKNOWN = 0;
  IEJAS_IMAGE_CS_GRAY    = 1;    // Standard Gray
  IEJAS_IMAGE_CS_RGB     = 2;    // Standard RGB
  IEJAS_IMAGE_CS_YCBCR   = 3;    // Standard YCC

  IEJAS_IMAGE_CT_RGB_R    = 0;
  IEJAS_IMAGE_CT_RGB_G    = 1;
  IEJAS_IMAGE_CT_RGB_B    = 2;
  IEJAS_IMAGE_CT_YCBCR_Y  = 0;
  IEJAS_IMAGE_CT_YCBCR_CB = 1;
  IEJAS_IMAGE_CT_YCBCR_CR = 2;
  IEJAS_IMAGE_CT_GRAY_Y   = 0;

  IEJAS_IMAGE_CT_OPACITY  = $8000;



procedure IEInitialize_iej2000; forward;





///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// static Jasper library wrappers


{$ifndef IEUSEDLLJPEG2000LIB}




{$RANGECHECKS OFF}

{$Z4}

type

  pjas_stream_t = pointer;
  pjas_image_t  = pointer;
  pjas_matrix_t = pointer;

  jas_image_cmptparm_t = record
    tlx: dword; //* The x-coordinate of the top-left corner of the component. */
    tly: dword; //* The y-coordinate of the top-left corner of the component. */
    hstep: dword; //* The horizontal sampling period in units of the reference grid. */
    vstep: dword; //* The vertical sampling period in units of the reference grid. */
    width: dword; //* The width of the component in samples. */
    height: dword; //* The height of the component in samples. */
    prec: word; //* The precision of the component sample data. */
    sgnd: integer; //* The signedness of the component sample data. */
  end;

  pjas_image_cmptparm_t = ^jas_image_cmptparm_t;
  jas_image_cmptparm_t_array = array[0..8192] of jas_image_cmptparm_t;
  pjas_image_cmptparm_t_array = ^jas_image_cmptparm_t_array;

  /////// from jas

  // initialization
procedure jas_init; external;

// streams
function jas_stream_fopen(filename, omode: PAnsiChar): pjas_stream_t; external;
procedure jas_stream_close(jstream: pjas_stream_t); external;
procedure jas_stream_flush(jstream: pjas_stream_t); external;

// Get the format of image data in a stream.
function jas_image_getfmt(jstream: pjas_stream_t): integer; external;

// Create an image from a stream in some specified format.
function jas_image_decode(jstream: pjas_stream_t; infmt: integer; inopts: PAnsiChar): pjas_image_t; external;

// Write an image to a stream in a specified format.
function jas_image_encode(image: pjas_image_t; jstream: pjas_stream_t; fmt: integer; optstr: PAnsiChar): integer; external;

// Delete a component from an image.
procedure jas_image_delcmpt(image: pjas_image_t; i: word); external;

// Read a rectangular region of an image component.
// The position and size of the rectangular region to be read is specified
// relative to the component's coordinate system.
function jas_image_readcmpt(image: pjas_image_t; cmptno: word; x, y, width, height: integer; data: pjas_matrix_t): integer; external;

// matrix (components)
function jas_matrix_create(numrows, numcols: integer): pjas_matrix_t; external;
procedure jas_matrix_destroy(matrix: pjas_matrix_t); external;

// Deallocate any resources associated with an image.
procedure jas_image_destroy(image: pjas_image_t); external;

// Clear the table of image formats.
procedure jas_image_clearfmts; external;

// Get the ID for the image format with the specified name.
function jas_image_strtofmt(s: PAnsiChar): integer; external;

// Get the name of the image format with the specified ID.
function jas_image_fmttostr(fmt: integer): PAnsiChar; external;

// Create an image.
function jas_image_create(numcmpts: word; cmptparms: pjas_image_cmptparm_t_array; colormodel: integer): pjas_image_t; external;

function jas_image_create0: pjas_image_t; external;

function jas_image_addcmpt(image: pjas_image_t; cmptno: word; cmptparm: pjas_image_cmptparm_t): integer; external;

function jas_image_getcmptbytype(image: pjas_image_t; ctype: integer): integer; external;

function jas_getdbglevel: integer; external;

function jas_setdbglevel(dbglevel: integer): integer; external;

procedure jas_image_writecmptsample(image: pjas_image_t; cmptno: integer; x, y: integer; v: dword); external;

procedure iejas_image_setcmpttype(image: pjas_image_t; cmptno: integer; ctype: integer); external;

//// end from jas

//// from xlib

// [IMAGE] Get the number of image components.
function iejas_image_numcmpts(image: pjas_image_t): integer; external;

// [IMAGE] The x-coordinate of the top-left corner of the image bounding box.
function iejas_image_getleft(image: pjas_image_t): integer; external;

// [IMAGE] The y-coordinate of the top-left corner of the image bounding box.
function iejas_image_gettop(image: pjas_image_t): integer; external;

// [IMAGE] The x-coordinate of the bottom-right corner of the image bounding box (plus one).
function iejas_image_getright(image: pjas_image_t): integer; external;

// [IMAGE] The y-coordinate of the bottom-right corner of the image bounding box (plus one).
function iejas_image_getbottom(image: pjas_image_t): integer; external;

// [MATRIX] get i,j value of the
function iejas_matrix_get(matrix: pjas_matrix_t; i, j: integer): integer; external;

// [IMAGE] Get the width of a component.
function iejas_image_cmptwidth(image: pjas_image_t; compno: integer): integer; external;

// [IMAGE] Get the height of a component.
function iejas_image_cmptheight(image: pjas_image_t; compno: integer): integer; external;

// [IMAGE] Get the precision of the sample data for a component.
function iejas_image_cmptprec(image: pjas_image_t; compno: integer): integer; external;

// [IMAGE] Get the width of the image in units of the image reference grid.
function iejas_image_width(image: pjas_image_t): integer; external;

// [IMAGE] Get the height of the image in units of the image reference grid.
function iejas_image_height(image: pjas_image_t): integer; external;

// [IMAGE] Get the color model used by the image.
function iejas_image_colorspace(image: pjas_image_t): integer; external;

procedure iejas_image_setcolorspace(image: pjas_image_t; colorspace: integer); external;

// [IMAGE] Get the x-coordinate of the top-left corner of a component.
function iejas_image_cmpttlx(image: pjas_image_t; cmptno: integer): integer; external;

// [IMAGE] Get the y-coordinate of the top-left corner of a component.
function iejas_image_cmpttly(image: pjas_image_t; cmptno: integer): integer; external;

// [IMAGE] Get the horizontal subsampling factor for a component
function iejas_image_cmpthstep(image: pjas_image_t; cmptno: integer): integer; external;

// [IMAGE] Get the vertical subsampling factor for a component.
function iejas_image_cmptvstep(image: pjas_image_t; cmptno: integer): integer; external;

///// end xlib

// color spaces
const



  JPC_NUMAGGCTXS = 1;
  JPC_NUMZCCTXS = 9;
  JPC_NUMMAGCTXS = 3;
  JPC_NUMSCCTXS = 5;
  JPC_NUMUCTXS = 1;
  JPC_AGGCTXNO = 0;
  JPC_ZCCTXNO = (JPC_AGGCTXNO + JPC_NUMAGGCTXS);
  JPC_MAGCTXNO = (JPC_ZCCTXNO + JPC_NUMZCCTXS);
  JPC_SCCTXNO = (JPC_MAGCTXNO + JPC_NUMMAGCTXS);
  JPC_UCTXNO = (JPC_SCCTXNO + JPC_NUMSCCTXS);
  JPC_NUMCTXS = (JPC_UCTXNO + JPC_NUMUCTXS);

var
  _jpc_zcctxnolut: array[0..4 * 256] of integer;
  _jpc_spblut: array[0..256] of integer;
  _jpc_scctxnolut: array[0..256] of integer;
  _jpc_magctxnolut: array[0..4096] of integer;
  _jpc_signmsedec: array[0..128] of integer;
  _jpc_refnmsedec: array[0..128] of integer;
  _jpc_signmsedec0: array[0..128] of integer;
  _jpc_refnmsedec0: array[0..128] of integer;
  _jpc_mqctxs: array[0..JPC_NUMCTXS] of integer;


///////////////////////////////////////////////////////

function _malloc(size: Integer): Pointer; cdecl;
begin
  result := allocmem(size);
end;

procedure _free(P: Pointer); cdecl;
begin
  FreeMem(P);
end;

function _memset(P: Pointer; B: Byte; count: Integer): pointer; cdecl;
begin
  FillChar(P^, count, B);
  result := P;
end;

function _memcpy(dest, source: Pointer; count: Integer): pointer; cdecl;
begin
  Move(source^, dest^, count);
  result := dest;
end;

function __ftol: integer;
var
  f: double;
begin
  asm
    lea    eax, f             //  BC++ passes floats on the FPU stack
    fstp  qword ptr [eax]     //  Delphi passes floats on the CPU stack
  end;
  if f > 2147483647.0 then
    f := 2147483647.0;
  if f < -2147483648.0 then
    f := 2147483648.0;
  result := integer(Trunc(f));
end;

procedure __assert(__cond: PAnsiChar; __file: PAnsiChar; __line: integer); cdecl;
begin
end;

procedure _abort; cdecl;
begin
end;

procedure _iejdebug(p: PAnsiChar); cdecl;
begin
  //
  //outputdebugstring(p);
end;

function _memmove(dest, source: Pointer; count: Integer): pointer; cdecl;
begin
  Move(source^, dest^, count);
  result := dest;
end;

function _strlen(str: PAnsiChar): integer; cdecl;
begin
  result := IEStrLen(str);
end;

function _realloc(block: pointer; size: integer): pointer; cdecl;
begin
  reallocmem(block, size);
  result := block;
end;

function _fscanf(f: pointer; format: PAnsiChar): integer; cdecl;
begin
  result := 0;
end;

// not used in Jasper
procedure _unlink; cdecl;
begin
end;

// not used in Jasper
procedure _setmode; cdecl;
begin
end;

// not used in Jasper
procedure _fputc; cdecl;
begin
end;

type
  TIETmpStream = class(TMemoryStream)
    public
      constructor Create;
      destructor Destroy; override;
  end;

constructor TIETmpStream.Create;
begin
  inherited Create;
end;

destructor TIETmPStream.Destroy;
begin
  inherited;
end;

function _open(path: PAnsiChar; access, mode: integer): integer; cdecl;
begin
  if path = 'TEMPSTREAM' then
    result := integer(TIETmpStream.Create)
  else
    result := integer(path);
end;

function _read(stream: integer; buf: pointer; len: integer): integer; cdecl;
var
  st: TStream;
begin
  st := TStream(pointer(stream));
  result := st.Read(pbyte(buf)^, len);
end;

function _close(stream: integer): integer; cdecl;
var
  st: tstream;
begin
  result := 0;
  st := TStream(pointer(stream));
  if (st is TIETmpStream) then
  begin
    FreeAndNil(st);
  end;
end;

function _write(stream: integer; buf: pointer; len: integer): integer; cdecl;
var
  st: tstream;
begin
  st := TStream(pointer(stream));
  result := st.Write(pbyte(buf)^, len);
end;

function _lseek(stream: integer; offset: integer; fromwhere: integer): integer; cdecl;
var
  st: tstream;
begin
  st := TStream(pointer(stream));
  case fromwhere of
    0: // SEEK_SET
      result := st.Seek(offset, soBeginning);
    1: // SEEK_CUR
      result := st.Seek(offset, soCurrent);
    2: // SEEK_END
      result := st.Seek(offset, soEnd);
  else
    result := -1;
  end;
end;

// we suppose that s is not null
function _tmpnam(s: PAnsiChar): PAnsiChar; cdecl;
begin
  result := s;
  IEStrPCopy(s, 'TEMPSTREAM');
end;

// used only for stdio io
procedure _fread; cdecl;
begin
end;

// used only for stdio io
function _fwrite(buf: PAnsiChar; size, n: integer; fil: pointer): integer; cdecl;
begin
  result := size * n;
end;

// used only for stdio io
procedure _fseek; cdecl;
begin
end;

// used only for stdio io
procedure _fclose; cdecl;
begin
end;

function _isspace(c: integer): integer; cdecl;
begin
  result := integer(c <= 32);
end;

function _isalpha(c: integer): integer; cdecl;
begin
  result := integer(((c >= 65) and (c <= 90)) or ((c >= 97) and (c <= 122)) or (c = 95));
end;

function _isdigit(c: integer): integer; cdecl;
begin
  result := integer((c >= 48) and (c <= 57));
end;

function _atol(s: PAnsiChar): integer; cdecl;
begin
  result := IEStrToIntDef(s, 0);
end;

function _strchr(s: PAnsiChar; c: integer): PAnsiChar; cdecl;
begin
  result := IEStrScan(s, AnsiChar(c));
end;

function _atof(s: PAnsiChar): double; cdecl;
var
  q: AnsiString;
  p1: integer;
begin
  q := AnsiString(s);
  p1 := IEPos(' ', q);
  if p1 = 0 then
    p1 := length(q) + 1;
  setlength(q, p1 - 1);
  result := IEStrToFloatDefA(q, 0);
end;

function _sqrt(x: double): double; cdecl;
begin
  result := sqrt(x);
end;

function _strrchr(s: PAnsiChar; c: integer): PAnsiChar; cdecl;
begin
  result := IEStrRScan(s, AnsiChar(c));
end;

function _isprint(c: integer): integer; cdecl;
begin
  result := integer(c > 31);
end;

function _strncpy(dest, src: PAnsiChar; maxlen: integer): PAnsiChar; cdecl;
begin
  result := IEStrMove(dest, src, maxlen);
end;

procedure __llmul;
asm
    push  edx
    push  eax
    mov    eax, [esp+16]
    mul    dword ptr [esp]
    mov    ecx, eax
    mov    eax, [esp+4]
    mul    dword ptr [esp+12]
    add    ecx, eax
    mov    eax, [esp]
    mul    dword ptr [esp+12]
    add    edx, ecx
    pop    ecx
    pop    ecx
    ret    8
end;

procedure __lldiv;
asm
    push    ebp
    push    ebx
    push    esi
    push    edi
    xor    edi,edi
    mov     ebx,20[esp]
    mov     ecx,24[esp]
    or      ecx,ecx
    jnz     @__lldiv@slow_ldiv
    or      edx,edx
    jz      @__lldiv@quick_ldiv
    or      ebx,ebx
    jz      @__lldiv@quick_ldiv
@__lldiv@slow_ldiv:
    or      edx,edx
    jns     @__lldiv@onepos
    neg     edx
    neg     eax
    sbb     edx,0
    or      edi,1
@__lldiv@onepos:
    or      ecx,ecx
    jns     @__lldiv@positive
    neg     ecx
    neg     ebx
    sbb     ecx,0
    xor    edi,1
@__lldiv@positive:
    mov     ebp,ecx
    mov     ecx,64
    push    edi
    xor     edi,edi
    xor     esi,esi
@__lldiv@xloop:
    shl     eax,1
    rcl     edx,1
    rcl     esi,1
    rcl     edi,1
    cmp     edi,ebp
    jb      @__lldiv@nosub
    ja      @__lldiv@subtract
    cmp     esi,ebx
    jb      @__lldiv@nosub
@__lldiv@subtract:
    sub     esi,ebx
    sbb     edi,ebp
    inc     eax
@__lldiv@nosub:
    loop    @__lldiv@xloop
    pop     ebx
    test    ebx,1
    jz      @__lldiv@finish
    neg     edx
    neg     eax
    sbb     edx,0
@__lldiv@finish:
    pop     edi
    pop     esi
    pop     ebx
    pop     ebp
    ret     8
@__lldiv@quick_ldiv:
    div     ebx
    xor     edx,edx
    jmp     @__lldiv@finish
end;

procedure jpc_seglist_remove; external;
procedure jpc_seg_destroy; external;
procedure jpc_seglist_insert; external;
procedure jpc_decode; external;
procedure jp2_decode; external;
procedure jp2_encode; external;
procedure jp2_validate; external;
procedure jpc_seg_alloc; external;
procedure jas_stream_puts; external;

{$L jp2_enc.obj}
{$L jpc_enc.obj}
{$L jpc_dec.obj}
{$L jpc_t1dec.obj}
{$L jpc_t1enc.obj}
{$L jpc_t2enc.obj}
{$L jpc_t2dec.obj}
{$L jpc_t2cod.obj}
{$L jpc_t1cod.obj}
{$L jpc_tsfb.obj}
{$L jpc_qmfb.obj}
{$L jpc_mct.obj}
{$L jpc_bs.obj}
{$L jas_getopt.obj}
{$L jp2_dec.obj}
{$L jas_init.obj}
{$L jpc_mqdec.obj}
{$L jpc_mqenc.obj}
{$L jpc_mqcod.obj}
{$L jas_tvp.obj}
{$L jp2_cod.obj}
{$L jas_image.obj}
{$L jpc_cs.obj}
{$L jas_seq.obj}
{$L jas_malloc.obj}
{$L jas_stream.obj}
{$L jas_string.obj}
{$L jas_version.obj}
{$L jpc_math.obj}
{$L jpc_util.obj}
{$L jpc_tagtree.obj}
{$L jas_debug.obj}

{$L xlibcj2.obj}


//////////////////////////////////////////////////////

type

  IEJP2K_Image               = pjas_image_t;
  IEJP2K_Matrix              = pjas_matrix_t;
  IEJP2K_ComponentParamsList = array of jas_image_cmptparm_t;

procedure IEJP2K_initialize();
begin
  jas_init();
end;

procedure IEJP2K_finalize();
begin
  jas_image_clearfmts();
end;

function IEJP2K_imageCreate(stream: TStream): IEJP2K_Image; overload;
var
  js: pjas_stream_t;
begin
  js := jas_stream_fopen(pointer(stream), 'rb');
  try
    result := jas_image_decode(js, -1, nil);
  finally
    jas_stream_close(js);
  end;
end;

function IEJP2K_imageCreate(numComponents: word; parameters: IEJP2K_ComponentParamsList; colorModel: integer): IEJP2K_Image; overload;
begin
  result := jas_image_create(numComponents, @parameters[0], colorModel);
end;

procedure IEJP2K_imageDestroy(var image: IEJP2K_image);
begin
  if image <> nil then
  begin
    jas_image_destroy(image);
    image := nil;
  end;
end;

procedure IEJP2K_imageEncode(image: IEJP2K_image; stream: TStream; format: integer; options: string);
var
  js: pjas_stream_t;
begin
  js := jas_stream_fopen(pointer(Stream), 'w+b');
  try
    jas_image_encode(image, js, format, PAnsiChar(AnsiString(options)));
    jas_stream_flush(js);
  finally
    jas_stream_close(js);
  end;
end;

function IEJP2K_getImageWidth(image: IEJP2K_Image): integer;
begin
  result := iejas_image_width(image);
end;

function IEJP2K_getImageHeight(image: IEJP2K_Image): integer;
begin
  result := iejas_image_height(image);
end;

function IEJP2K_getColorSpace(image: IEJP2K_Image): integer;
begin
  result := iejas_image_colorspace(image);
end;

// note: component can be JAS_IMAGE_CT_RGB_R, etc... no need to call JAS_IMAGE_CT_COLOR
function IEJP2K_getComponentByType(image: IEJP2K_Image; component: integer): integer;
begin
  result := jas_image_getcmptbytype(image, (component and $7FFF));
end;

// note: component can be JAS_IMAGE_CT_RGB_R, etc... no need to call JAS_IMAGE_CT_COLOR
procedure IEJP2K_setComponentType(image: IEJP2K_Image; index: integer; component: integer);
begin
  iejas_image_setcmpttype(image, index, (component and $7FFF));
end;

function IEJP2K_getComponentPrecision(image: IEJP2K_Image; index: integer): integer;
begin
  result := iejas_image_cmptprec(image, index);
end;

function IEJP2K_getNumComponents(image: IEJP2K_Image): integer;
begin
  result := iejas_image_numcmpts(image);
end;

function IEJP2K_getComponentHeight(image: IEJP2K_Image; index: integer): integer;
begin
  result := iejas_image_cmptheight(image, index);
end;

function IEJP2K_getComponentWidth(image: IEJP2K_Image; index: integer): integer;
begin
  result := iejas_image_cmptwidth(image, index);
end;

function IEJP2K_matrixCreate(numRows: integer; numCols: integer): IEJP2K_Matrix;
begin
  result := jas_matrix_create(numRows, numCols);
end;

procedure IEJP2K_matrixDestroy(var matrix: IEJP2K_Matrix);
begin
  jas_matrix_destroy(matrix);
  matrix := nil;
end;

function IEJP2K_matrixGetValue(matrix: IEJP2K_Matrix; i: integer; j: integer): integer;
begin
  result := iejas_matrix_get(matrix, i, j);
end;

function IEJP2K_readComponent(image: IEJP2K_Image; index: word; x: integer; y: integer; width: integer; height: integer; data: IEJP2K_Matrix): integer;
begin
  result := jas_image_readcmpt(image, index, x, y, width, height, data);
end;

function IEJP2K_getComponentTopLeftY(image: IEJP2K_Image; index: integer): integer;
begin
  result := iejas_image_cmpttly(image, index);
end;

function IEJP2K_getComponentTopLeftX(image: IEJP2K_Image; index: integer): integer;
begin
  result := iejas_image_cmpttlx(image, index);
end;

function IEJP2K_getComponentVStep(image: IEJP2K_Image; index: integer): integer;
begin
  result := iejas_image_cmptvstep(image, index);
end;

function IEJP2K_getComponentHStep(image: IEJP2K_Image; index: integer): integer;
begin
  result := iejas_image_cmpthstep(image, index);
end;

function IEJP2K_createComponentParametersList(size: integer): IEJP2K_ComponentParamsList;
begin
  SetLength(result, size);
end;

procedure IEJP2K_destroyComponentParametersList(var parameters: IEJP2K_ComponentParamsList);
begin
  SetLength(parameters, 0);
end;

procedure IEJP2K_setComponentParameters(parameters: IEJP2K_ComponentParamsList; index: integer; tlx: dword; tly: dword; hstep: dword; vstep: dword; width: dword; height: dword; prec: word; sgnd: integer);
begin
  parameters[index].tlx    := tlx;
  parameters[index].tly    := tly;
  parameters[index].hstep  := hstep;
  parameters[index].vstep  := vstep;
  parameters[index].width  := width;
  parameters[index].height := height;
  parameters[index].prec   := prec;
  parameters[index].sgnd   := sgnd;
end;

procedure IEJP2K_writeComponentSample(image: IEJP2K_Image; index: integer; x: integer; y: integer; v: dword);
begin
  jas_image_writecmptsample(image, index, x, y, v);
end;

procedure IEJP2K_writeRowRGB8(image: IEJP2K_Image; width: integer; rowIndex: integer; bgr8Array: PRGB; alphaArray: pbyte; colors: integer);
var
  x: integer;
  mul: double;
begin
  mul := colors / 255;
  for x := 0 to width - 1 do
  begin
    with bgr8Array^ do
    begin
      IEJP2K_writeComponentSample(image, 0, x, rowIndex, trunc(r * mul));
      IEJP2K_writeComponentSample(image, 1, x, rowIndex, trunc(g * mul));
      IEJP2K_writeComponentSample(image, 2, x, rowIndex, trunc(b * mul));
    end;
    if alphaArray <> nil then
    begin
      IEJP2K_writeComponentSample(image, 3, x, rowIndex, trunc(alphaArray^ * mul));
      inc(alphaArray);
    end;
    inc(bgr8Array);
  end;
end;

procedure IEJP2K_readLinearBGR8(image: IEJP2K_Image; blueMatrix: IEJP2K_Matrix; greenMatrix: IEJP2K_Matrix; redMatrix: IEJP2K_Matrix; rowIndex: integer; bluePrec: integer; greenPrec: integer; redPrec: integer; destBGR8: pbyte; width: integer);
var
  j: integer;
begin
  for j := 0 to width - 1 do
  begin
    destBGR8^ := (IEJP2K_matrixGetValue(blueMatrix, rowIndex, j) shl (32 - bluePrec)) shr 24;
    inc(destBGR8);
    destBGR8^ := (IEJP2K_matrixGetValue(greenMatrix, rowIndex, j) shl (32 - greenPrec)) shr 24;
    inc(destBGR8);
    destBGR8^ := (IEJP2K_matrixGetValue(redMatrix, rowIndex, j) shl (32 - redPrec)) shr 24;
    inc(destBGR8);
  end;
end;


{$endif} // IEUSEDLLJPEG2000LIB


///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////








///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// dynamic (DLL) Jasper library wrappers

{$ifdef IEUSEDLLJPEG2000LIB}


type

  IEJP2K_Image               = TIELibJP2KImage;
  IEJP2K_Matrix              = TIELibJP2KMatrix;
  IEJP2K_ComponentParamsList = TIELibJP2KComponentParamsList;


procedure IEJP2K_initialize();
begin
  if IELibAvailable() then
    IELib.JP2KInitialize()
  else
    raise EIEJPEG2000Exception.Create(IERS_IEVISIONNOTFOUND);
end;

procedure IEJP2K_finalize();
begin
  if IELibAvailable() then
    IELib.JP2KFinalize();
end;

function IEJP2K_imageCreate(stream: TStream): IEJP2K_Image; overload;
begin
  result := IELib.createJP2KImage(IELib.createCustomStream(TIEVCLStreamProvider.Create(stream)));  // TIEVCLStreamProvider will be freed by IEJP2K_Image (that is TIELibJP2KImage)
end;

function IEJP2K_imageCreate(numComponents: word; parameters: IEJP2K_ComponentParamsList; colorModel: integer): IEJP2K_Image; overload;
begin
  result := IELib.createJP2KImage(numComponents, parameters, colorModel);
end;

procedure IEJP2K_imageDestroy(var image: IEJP2K_image);
begin
  image := nil;
end;

procedure IEJP2K_imageEncode(image: IEJP2K_image; stream: TStream; format: integer; options: string);
begin
  image.encode(IELib.createCustomStream(TIEVCLStreamProvider.Create(stream)), format, PAnsiChar(AnsiString(options)));   // TIEVCLStreamProvider will be freed by IEJP2K_Image (that is TIELibJP2KImage)
end;

function IEJP2K_getImageWidth(image: IEJP2K_Image): integer;
begin
  result := image.getWidth();
end;

function IEJP2K_getImageHeight(image: IEJP2K_Image): integer;
begin
  result := image.getHeight();
end;

function IEJP2K_getColorSpace(image: IEJP2K_Image): integer;
begin
  result := image.getColorSpace();
end;

// note: component can be JAS_IMAGE_CT_RGB_R, etc... no need to call JAS_IMAGE_CT_COLOR
function IEJP2K_getComponentByType(image: IEJP2K_Image; component: integer): integer;
begin
  result := image.getComponentByType(component);
end;

// note: component can be JAS_IMAGE_CT_RGB_R, etc... no need to call JAS_IMAGE_CT_COLOR
procedure IEJP2K_setComponentType(image: IEJP2K_Image; index: integer; component: integer);
begin
  image.setComponentType(index, component);
end;

function IEJP2K_getComponentPrecision(image: IEJP2K_Image; index: integer): integer;
begin
  result := image.getComponentPrecision(index);
end;

function IEJP2K_getNumComponents(image: IEJP2K_Image): integer;
begin
  result := image.getNumComponents();
end;

function IEJP2K_getComponentHeight(image: IEJP2K_Image; index: integer): integer;
begin
  result := image.getComponentHeight(index);
end;

function IEJP2K_getComponentWidth(image: IEJP2K_Image; index: integer): integer;
begin
  result := image.getComponentWidth(index);
end;

function IEJP2K_matrixCreate(numRows: integer; numCols: integer): IEJP2K_Matrix;
begin
  result := IELib.createJP2KMatrix(numRows, numCols);
end;

procedure IEJP2K_matrixDestroy(var matrix: IEJP2K_Matrix);
begin
  matrix := nil;
end;

function IEJP2K_matrixGetValue(matrix: IEJP2K_Matrix; i: integer; j: integer): integer;
begin
  result := matrix.getValue(i, j);
end;

function IEJP2K_readComponent(image: IEJP2K_Image; index: word; x: integer; y: integer; width: integer; height: integer; data: IEJP2K_Matrix): integer;
begin
  result := image.readComponent(index, x, y, width, height, data);
end;

function IEJP2K_getComponentTopLeftY(image: IEJP2K_Image; index: integer): integer;
begin
  result := image.getComponentTopLeftY(index);
end;

function IEJP2K_getComponentTopLeftX(image: IEJP2K_Image; index: integer): integer;
begin
  result := image.getComponentTopLeftX(index);
end;

function IEJP2K_getComponentVStep(image: IEJP2K_Image; index: integer): integer;
begin
  result := image.getComponentVStep(index);
end;

function IEJP2K_getComponentHStep(image: IEJP2K_Image; index: integer): integer;
begin
  result := image.getComponentHStep(index);
end;

function IEJP2K_createComponentParametersList(size: integer): IEJP2K_ComponentParamsList;
begin
  result := IELib.createJP2KComponentParamsList(size);
end;

procedure IEJP2K_destroyComponentParametersList(var parameters: IEJP2K_ComponentParamsList);
begin
  parameters := nil;
end;

procedure IEJP2K_setComponentParameters(parameters: IEJP2K_ComponentParamsList; index: integer; tlx: dword; tly: dword; hstep: dword; vstep: dword; width: dword; height: dword; prec: word; sgnd: integer);
begin
  parameters.assign(index, tlx, tly, hstep, vstep, width, height, prec, sgnd);
end;

procedure IEJP2K_writeComponentSample(image: IEJP2K_Image; index: integer; x: integer; y: integer; v: dword);
begin
  image.writeComponentSample(index, x, y, v);
end;

procedure IEJP2K_writeRowRGB8(image: IEJP2K_Image; width: integer; rowIndex: integer; bgr8Array: PRGB; alphaArray: pbyte; colors: integer);
begin
  image.writeRowRGB8(width, rowIndex, pbyte(bgr8Array), alphaArray, colors);
end;

procedure IEJP2K_readLinearBGR8(image: IEJP2K_Image; blueMatrix: IEJP2K_Matrix; greenMatrix: IEJP2K_Matrix; redMatrix: IEJP2K_Matrix; rowIndex: integer; bluePrec: integer; greenPrec: integer; redPrec: integer; destBGR8: pbyte; width: integer);
begin
  image.readLinearBGR8(blueMatrix, greenMatrix, redMatrix, rowIndex, bluePrec, greenPrec, redPrec, destBGR8, width);
end;


{$endif}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////



function vctocc(i, co, cs: integer): integer;
begin
  result := (i - co) div cs;
end;

// return true if is a jp2 stream
function J2KTryStreamJP2(Stream: TStream): boolean;
var
  base: int64;
  dw1, dw2, dw3: dword;
begin
  base := Stream.position;
  result := false;
  // try jp2 - try jp2 signature
  Stream.read(dw1, 4); // LBox
  Stream.read(dw2, 4); // TBox
  Stream.read(dw3, 4); // DBox
  dw1 := IESwapDWord(dw1);
  dw2 := IESwapDWord(dw2);
  dw3 := IESwapDWord(dw3);
  if (dw1 = 12) and (dw2 = $6A502020) and (dw3 = $0D0A870A) then
    result := true;

  Stream.position := base;
end;

// return true if is a jpc or j2k stream
function J2KTryStreamJ2K(Stream: TStream): boolean;
var
  base: int64;
  w1, w2: word;
begin
  base := Stream.position;
  result := false;
  // try j2k, jpc -SOC, SIZ
  Stream.read(w1, 2); // SOC
  Stream.read(w2, 2); // SIZ
  w1 := IESwapWord(w1);
  w2 := IESwapWord(w2);
  if (w1 = $FF4F) and (w2 = $FF51) then
    result := true;

  Stream.position := base;
end;


var
  CrToRedTable, CbToBlueTable, CrToGreenTable, CbToGreenTable: array[0..255] of Integer;
  YCbCrCoefficients: array[0..2] of Single;


procedure CreateYCbCrLookup;
var
  F1, F2, F3, F4: Single;
  LumaRed,
  LumaGreen,
  LumaBlue: Single;
  I: Integer;
  Offset1: Integer;
begin
  YCbCrCoefficients[0] := 0.299;
  YCbCrCoefficients[1] := 0.587;
  YCbCrCoefficients[2] := 0.114;
  LumaRed   := YCbCrCoefficients[0];
  LumaGreen := YCbCrCoefficients[1];
  LumaBlue  := YCbCrCoefficients[2];
  F1 := 2 - 2 * LumaRed;
  F2 := LumaRed * F1 / LumaGreen;
  F3 := 2 - 2 * LumaBlue;
  F4 := LumaBlue * F3 / LumaGreen;
  Offset1 := -128;
  for I := 0 to 255 do
  begin
    CrToRedTable[I]   := Round(F1 * Offset1);
    CbToBlueTable[I]  := Round(F3 * Offset1);
    CrToGreenTable[I] := -Round(F2 * Offset1);
    CbToGreenTable[I] := -Round(F4 * Offset1);
    Inc(Offset1);
  end;
end;


procedure J2KReadStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var xProgress: TProgressRec; Preview: boolean);
var
  im: IEJP2K_Image;
  numcmpts, numc: integer;
  i, j, q: integer;
  prow: pbyte;
  image_width, image_height: integer;
  cmp: array [0..10] of integer;
  mt: array [0..10] of IEJP2K_Matrix;
  v: array [0..3] of integer;
  u: array [0..3] of double;
  vi: integer;
  arx0, ary0: pintegerarray;
  arx1, ary1: pintegerarray;
  arx2, ary2: pintegerarray;
  isLinear: boolean;
  bStream: TIEBufferedReadStream;
  pb: pbyte;
  pw: pword;
begin
  IEInitialize_iej2000();

  if (not J2KTryStreamJP2(Stream)) and (not J2KTryStreamJ2K(Stream)) then
  begin
    xProgress.Aborting^ := true;
    exit;
  end;

  im := nil;

  try

    bStream := TIEBufferedReadStream.Create(Stream, 8192);
    try
      im := IEJP2K_imageCreate(bStream);
    finally
      bStream.Free();
    end;

    if im = nil then
    begin
      xProgress.Aborting^ := true;
      exit;
    end;

    numcmpts := IEJP2K_getNumComponents(im); // samples per pixel

    image_width  := IEJP2K_getImageWidth(im);
    image_height := IEJP2K_getImageHeight(im);
    numc := 3;
    IOParams.ImageCount := 1;
    case IEJP2K_getColorSpace(im) of
      IEJAS_IMAGE_CS_RGB:
        begin
          cmp[0] := IEJP2K_getComponentByType(im, IEJAS_IMAGE_CT_RGB_R);
          if cmp[0] > 255 then
            cmp[0] := 0;
          cmp[1] := IEJP2K_getComponentByType(im, IEJAS_IMAGE_CT_RGB_G);
          if cmp[1] > 255 then
            cmp[1] := 1;
          cmp[2] := IEJP2K_getComponentByType(im, IEJAS_IMAGE_CT_RGB_B);
          if cmp[2] > 255 then
            cmp[2] := 2;

          // has alpha channel?
          if numcmpts = 4 then
          begin
            cmp[3] := IEJP2K_getComponentByType(im, IEJAS_IMAGE_CT_OPACITY and $7FFF);
            if cmp[3] > 255 then
              cmp[3] := 3;
          end;

          IOParams.BitsPerSample   := IEJP2K_getComponentPrecision(im, cmp[0]);
          IOParams.SamplesPerPixel := 3;
        end;
      IEJAS_IMAGE_CS_YCBCR:
        begin
          cmp[0] := IEJP2K_getComponentByType(im, IEJAS_IMAGE_CT_YCBCR_Y);
          if cmp[0] > 255 then
            cmp[0] := 0;
          cmp[1] := IEJP2K_getComponentByType(im, IEJAS_IMAGE_CT_YCBCR_CB);
          if cmp[1] > 255 then
            cmp[1] := 0;
          cmp[2] := IEJP2K_getComponentByType(im, IEJAS_IMAGE_CT_YCBCR_CR);
          if cmp[2] > 255 then
            cmp[2] := 0;
          IOParams.BitsPerSample   := IEJP2K_getComponentPrecision(im, cmp[0]);
          IOParams.SamplesPerPixel := 3;
        end;
      IEJAS_IMAGE_CS_GRAY:
        begin
          cmp[0] := IEJP2K_getComponentByType(im, IEJAS_IMAGE_CT_GRAY_Y);
          if cmp[0] > 255 then
            cmp[0] := 0;
          IOParams.BitsPerSample   := IEJP2K_getComponentPrecision(im, cmp[0]);
          IOParams.SamplesPerPixel := 1;
          numc := 1;
        end;
    else
      begin
        // unsupported color space
        xProgress.Aborting^ := true;
        IEJP2K_imageDestroy(im);
        exit;
      end;
    end;
    IOParams.DpiX := IEGlobalSettings().DefaultDPIX;
    IOParams.DpiY := IEGlobalSettings().DefaultDPIY;
    IOParams.FreeColorMap();
    IOParams.Width  := image_width;
    IOParams.Height := image_height;
    IOParams.OriginalWidth  := image_width;
    IOParams.OriginalHeight := image_height;

    if not Preview then
    begin

      for q := 0 to high(mt) do
        mt[q] := nil;
      arx0 := nil;
      ary0 := nil;
      arx1 := nil;
      ary1 := nil;
      arx2 := nil;
      ary2 := nil;

      try

        if numcmpts = 2 then
          numcmpts := 1;

        for q := 0 to numcmpts - 1 do
        begin
          mt[q] := IEJP2K_matrixCreate(IEJP2K_getComponentHeight(im, q), IEJP2K_getComponentWidth(im, q));
          IEJP2K_readComponent(im, q, 0, 0, IEJP2K_getComponentWidth(im, q), IEJP2K_getComponentHeight(im, q), mt[q]);
        end;

        if Bitmap.Location <> ieTBitmap then
        begin
          // native pixel format
          case IEJP2K_getColorSpace(im) of
            IEJAS_IMAGE_CS_RGB:
              begin
                Bitmap.Allocate(image_width, image_height, ie24RGB);
              end;
            IEJAS_IMAGE_CS_YCBCR:
              begin
                Bitmap.Allocate(image_width, image_height, ie24RGB);
              end;
            IEJAS_IMAGE_CS_GRAY:
              begin
                if IOParams.BitsPerSample = 1 then
                  Bitmap.Allocate(image_width, image_height, ie1g)
                else
                if IOParams.BitsPerSample = 8 then
                  Bitmap.Allocate(image_width, image_height, ie8g)
                else
                if IOParams.BitsPerSample > 8 then
                  Bitmap.Allocate(image_width, image_height, ie16g)
                else
                  Bitmap.Allocate(image_width, image_height, ie24RGB);  // unsupported native            
              end;
          end;
        end
        else
        begin
          // using TBitmap
          if (IOParams.BitsPerSample = 1) and (IOParams.SamplesPerPixel = 1) then
            Bitmap.Allocate(image_width, image_height, ie1g)
          else
            Bitmap.Allocate(image_width, image_height, ie24RGB);
        end;

        xProgress.per1 := 100 / image_height;

        isLinear := true;
        for q := 0 to imin(numc, numcmpts) - 1 do
        begin
          for i := 0 to image_height - 1 do
          begin
            if vctocc(i, IEJP2K_getComponentTopLeftY(im, cmp[q]), IEJP2K_getComponentVStep(im, cmp[q])) <> i then
            begin
              isLinear := false;
              break;
            end;
            for j := 0 to image_width -1 do
            begin
              if vctocc(j, IEJP2K_getComponentTopLeftX(im, cmp[0]), IEJP2K_getComponentHStep(im, cmp[0])) <> j then
              begin
                isLinear := false;
                break;
              end;
            end;
            if not isLinear then
              break;
          end;
          if not isLinear then
            break;
        end;

        if (not isLinear) and (numcmpts = 3) then
        begin
          getmem(arx0, sizeof(integer) * image_width);
          getmem(ary0, sizeof(integer) * image_height);
          getmem(arx1, sizeof(integer) * image_width);
          getmem(ary1, sizeof(integer) * image_height);
          getmem(arx2, sizeof(integer) * image_width);
          getmem(ary2, sizeof(integer) * image_height);
          for i := 0 to image_height - 1 do
          begin
            ary0[i] := vctocc(i, IEJP2K_getComponentTopLeftY(im, cmp[0]), IEJP2K_getComponentVStep(im, cmp[0]));
            if (ary0[i] < 0) or (ary0[i] >= IEJP2K_getComponentHeight(im, cmp[0])) then
              ary0[i] := 0;

            ary1[i] := vctocc(i, IEJP2K_getComponentTopLeftY(im, cmp[1]), IEJP2K_getComponentVStep(im, cmp[1]));
            if (ary1[i] < 0) or (ary1[i] >= IEJP2K_getComponentHeight(im, cmp[1])) then
              ary1[i] := 0;

            ary2[i] := vctocc(i, IEJP2K_getComponentTopLeftY(im, cmp[2]), IEJP2K_getComponentVStep(im, cmp[2]));
            if (ary2[i] < 0) or (ary2[i] >= IEJP2K_getComponentHeight(im, cmp[2])) then
              ary2[i] := 0;

            for j := 0 to image_width - 1 do
            begin
              arx0[j] := vctocc(j, IEJP2K_getComponentTopLeftX(im, cmp[0]), IEJP2K_getComponentHStep(im, cmp[0]));
              if (arx0[j] < 0) or (arx0[j] >= IEJP2K_getComponentWidth(im, cmp[0])) then
                arx0[j] := 0;

              arx1[j] := vctocc(j, IEJP2K_getComponentTopLeftX(im, cmp[1]), IEJP2K_getComponentHStep(im, cmp[1]));
              if (arx1[j] < 0) or (arx1[j] >= IEJP2K_getComponentWidth(im, cmp[1])) then
                arx1[j] := 0;

              arx2[j] := vctocc(j, IEJP2K_getComponentTopLeftX(im, cmp[2]), IEJP2K_getComponentHStep(im, cmp[2]));
              if (arx2[j] < 0) or (arx2[j] >= IEJP2K_getComponentWidth(im, cmp[2])) then
                arx2[j] := 0;
            end;
          end;
        end;

        if (not isLinear) and (numcmpts = 1) then
        begin
          getmem(arx0, sizeof(integer) * image_width);
          getmem(ary0, sizeof(integer) * image_height);
          for i := 0 to image_height - 1 do
          begin
            ary0[i] := vctocc(i, IEJP2K_getComponentTopLeftY(im, cmp[0]), IEJP2K_getComponentVStep(im, cmp[0]));
            if (ary0[i] < 0) or (ary0[i] >= IEJP2K_getComponentHeight(im, cmp[0])) then
              ary0[i] := 0;
            for j := 0 to image_width - 1 do
            begin
              arx0[j] := vctocc(j, IEJP2K_getComponentTopLeftX(im, cmp[0]), IEJP2K_getComponentHStep(im, cmp[0]));
              if (arx0[j] < 0) or (arx0[j] >= IEJP2K_getComponentWidth(im, cmp[0])) then
                arx0[j] := 0;
            end;
          end;
        end;

        if numcmpts = 3 then
        begin
          for i := 0 to image_height - 1 do
          begin
            prow := bitmap.ScanLine[i];
            case IEJP2K_getColorSpace(im) of
              IEJAS_IMAGE_CS_RGB:
                begin
                  if isLinear then
                  begin
                    IEJP2K_readLinearBGR8(im, mt[cmp[2]], mt[cmp[1]], mt[cmp[0]], i, IEJP2K_getComponentPrecision(im, cmp[2]), IEJP2K_getComponentPrecision(im, cmp[1]), IEJP2K_getComponentPrecision(im, cmp[0]), prow, image_width);
                  end
                  else
                  begin
                    for j := 0 to image_width - 1 do
                    begin
                      prow^ := (IEJP2K_matrixGetValue(mt[cmp[2]], ary2[i], arx2[j]) shl (32 - IEJP2K_getComponentPrecision(im, cmp[2]))) shr 24;
                      inc(prow);
                      prow^ := (IEJP2K_matrixGetValue(mt[cmp[1]], ary1[i], arx1[j]) shl (32 - IEJP2K_getComponentPrecision(im, cmp[1]))) shr 24;
                      inc(prow);
                      prow^ := (IEJP2K_matrixGetValue(mt[cmp[0]], ary0[i], arx0[j]) shl (32 - IEJP2K_getComponentPrecision(im, cmp[0]))) shr 24;
                      inc(prow);
                    end;
                  end;
                end;
              IEJAS_IMAGE_CS_YCBCR:
                begin
                  if isLinear then
                  begin
                    for j := 0 to image_width - 1 do
                    begin
                      v[0] := (IEJP2K_matrixGetValue(mt[cmp[0]], i, j) shl (32 - IEJP2K_getComponentPrecision(im, cmp[0]))) shr 24;
                      v[1] := (IEJP2K_matrixGetValue(mt[cmp[1]], i, j) shl (32 - IEJP2K_getComponentPrecision(im, cmp[1]))) shr 24;
                      v[2] := (IEJP2K_matrixGetValue(mt[cmp[2]], i, j) shl (32 - IEJP2K_getComponentPrecision(im, cmp[2]))) shr 24;
                      u[0] := blimit(v[0] + CrToRedTable[v[2]]);
                      u[1] := blimit(v[0] + CbToGreenTable[v[1]] + CrToGreentable[v[2]]);
                      u[2] := blimit(v[0] + CbToBlueTable[v[1]]);
                      v[0] := trunc(u[0]);
                      v[1] := trunc(u[1]);
                      v[2] := trunc(u[2]);
                      prow^ := blimit(v[2]);
                      inc(prow);
                      prow^ := blimit(v[1]);
                      inc(prow);
                      prow^ := blimit(v[0]);
                      inc(prow);
                    end;
                  end
                  else
                  begin
                    for j := 0 to image_width - 1 do
                    begin
                      v[0] := (IEJP2K_matrixGetValue(mt[cmp[0]], ary0[i], arx0[j]) shl (32 - IEJP2K_getComponentPrecision(im, cmp[0]))) shr 24;
                      v[1] := (IEJP2K_matrixGetValue(mt[cmp[1]], ary1[i], arx1[j]) shl (32 - IEJP2K_getComponentPrecision(im, cmp[1]))) shr 24;
                      v[2] := (IEJP2K_matrixGetValue(mt[cmp[2]], ary2[i], arx2[j]) shl (32 - IEJP2K_getComponentPrecision(im, cmp[2]))) shr 24;
                      u[0] := blimit(v[0] + CrToRedTable[v[2]]);
                      u[1] := blimit(v[0] + CbToGreenTable[v[1]] + CrToGreentable[v[2]]);
                      u[2] := blimit(v[0] + CbToBlueTable[v[1]]);
                      v[0] := trunc(u[0]);
                      v[1] := trunc(u[1]);
                      v[2] := trunc(u[2]);
                      prow^ := blimit(v[2]);
                      inc(prow);
                      prow^ := blimit(v[1]);
                      inc(prow);
                      prow^ := blimit(v[0]);
                      inc(prow);
                    end;
                  end;
                end;
            end;
            // OnProgress
            with xProgress do
              if assigned(fOnProgress) then
                fOnProgress(Sender, trunc(per1 * (i)));
            if xProgress.Aborting^ then
              break;
          end;
        end

        else
        if (numcmpts = 4) and isLinear then
        begin
          // alpha channel currently not supported (this is discarded now)
          for i := 0 to image_height - 1 do
          begin
            prow := bitmap.ScanLine[i];
            case IEJP2K_getColorSpace(im) of
              IEJAS_IMAGE_CS_RGB:
                begin
                  IEJP2K_readLinearBGR8(im, mt[cmp[2]], mt[cmp[1]], mt[cmp[0]], i, IEJP2K_getComponentPrecision(im, cmp[2]), IEJP2K_getComponentPrecision(im, cmp[1]), IEJP2K_getComponentPrecision(im, cmp[0]), prow, image_width);
                end;
            end;
            // OnProgress
            with xProgress do
              if assigned(fOnProgress) then
                fOnProgress(Sender, trunc(per1 * (i)));
            if xProgress.Aborting^ then
              break;
          end;
        end

        else
        if (numcmpts = 1) or (IEJP2K_getColorSpace(im) = IEJAS_IMAGE_CS_GRAY) then
        begin
          for i := 0 to image_height - 1 do
          begin
            case Bitmap.PixelFormat of
              ie1g:
                begin
                  prow := bitmap.ScanLine[i];
                  for j := 0 to image_width - 1 do
                  begin
                    if isLinear then
                      vi := (IEJP2K_matrixGetValue(mt[cmp[0]], i, j) shl (32 - IEJP2K_getComponentPrecision(im, cmp[0]))) shr 24
                    else
                      vi := (IEJP2K_matrixGetValue(mt[cmp[0]], ary0[i], arx0[j]) shl (32 - IEJP2K_getComponentPrecision(im, cmp[0]))) shr 24;
                    _SetPixelbw(pbyte(prow), j, vi);
                  end;
                end;
              ie8g:
                begin
                  pb := bitmap.ScanLine[i];
                  for j := 0 to image_width - 1 do
                  begin
                    if isLinear then
                      pb^ := (IEJP2K_matrixGetValue(mt[cmp[0]], i, j) shl (32 - IEJP2K_getComponentPrecision(im, cmp[0]))) shr 24
                    else
                      pb^ := (IEJP2K_matrixGetValue(mt[cmp[0]], ary0[i], arx0[j]) shl (32 - IEJP2K_getComponentPrecision(im, cmp[0]))) shr 24;
                    inc(pb);
                  end;
                end;
              ie16g:
                begin
                  pw := bitmap.ScanLine[i];
                  for j := 0 to image_width - 1 do
                  begin
                    if isLinear then
                      pw^ := (IEJP2K_matrixGetValue(mt[cmp[0]], i, j) shl (32 - IEJP2K_getComponentPrecision(im, cmp[0]))) shr 16
                    else
                      pw^ := (IEJP2K_matrixGetValue(mt[cmp[0]], ary0[i], arx0[j]) shl (32 - IEJP2K_getComponentPrecision(im, cmp[0]))) shr 16;
                    inc(pw);
                  end;
                end;
              ie24RGB:
                begin
                  prow := bitmap.ScanLine[i];
                  for j := 0 to image_width - 1 do
                  begin
                    if isLinear then
                      vi := (IEJP2K_matrixGetValue(mt[cmp[0]], i, j) shl (32 - IEJP2K_getComponentPrecision(im, cmp[0]))) shr 24
                    else
                      vi := (IEJP2K_matrixGetValue(mt[cmp[0]], ary0[i], arx0[j]) shl (32 - IEJP2K_getComponentPrecision(im, cmp[0]))) shr 24;
                    prow^ := vi;
                    inc(prow);
                    prow^ := vi;
                    inc(prow);
                    prow^ := vi;
                    inc(prow);
                  end;
                end;
            end;
            // OnProgress
            with xProgress do
              if assigned(fOnProgress) then
                fOnProgress(Sender, trunc(per1 * (i)));
            if xProgress.Aborting^ then
              break;
          end;
        end;

      finally

        for q := 0 to high(mt) do
          if mt[q] <> nil then
            IEJP2K_matrixDestroy(mt[q]);

        // free even when they are "nil"
        freemem(arx0);
        freemem(ary0);
        freemem(arx1);
        freemem(ary1);
        freemem(arx2);
        freemem(ary2);
      end;

    end; // not preview

  finally
    IEJP2K_imageDestroy(im);
  end;
end;


// fmt can be JAS_IMAGE_CM_GRAY(1), JAS_IMAGE_CM_RGB(2), JAS_IMAGE_CM_YCC(3)
// format: 0=jp2 1=j2k/jpc
procedure J2KWriteStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var xProgress: TProgressRec; format: integer);
var
  im: IEJP2K_Image;
  cmptparams: IEJP2K_ComponentParamsList;
  outopts: string;
  ww, hh, y, x, vv, colors: integer;
  row: PRGB;
  pb: pbyte;
  pw: pword;
  nullpr: TProgressRec;
  a: pbyte;
  HasAlpha: boolean;
  bStream: TIEBufferedWriteStream;
  RedToGrayCoef, GreenToGrayCoef, BlueToGrayCoef: integer;
begin
  IEInitialize_iej2000();

  with nullpr do
  begin
    Aborting    := xProgress.Aborting;
    fOnProgress := nil;
    Sender      := nil;
  end;

  im := nil;

  ww := Bitmap.Width;
  hh := Bitmap.Height;

  case Bitmap.PixelFormat of
    ie1g:
      begin
        IOParams.BitsPerSample    := 1;
        IOParams.J2000_ColorSpace := ioJ2000_GRAYLEV;
      end;
    ie8g:
      begin
        IOParams.BitsPerSample    := 8;
        IOParams.J2000_ColorSpace := ioJ2000_GRAYLEV;
      end;
    ie16g:
      begin
        IOParams.BitsPerSample    := 16;
        IOParams.J2000_ColorSpace := ioJ2000_GRAYLEV;
      end;
  end;

  colors := (1 shl IOParams.BitsPerSample) - 1;

  if hh = 0 then
    xProgress.per1 := 0
  else
    xProgress.per1 := 100 / hh;
  xProgress.val := 0;

  cmptparams := nil;

  case IOParams.J2000_ColorSpace of
    ioJ2000_GRAYLEV:
      begin
        // gray scale or black/white
        cmptparams := IEJP2K_createComponentParametersList(1);
        IEJP2K_setComponentParameters(cmptparams, 0, 0, 0, 1, 1, ww, hh, IOParams.BitsPerSample, 0);

        im := IEJP2K_imageCreate(1, cmptparams, IEJAS_IMAGE_CS_GRAY);

        IEJP2K_setComponentType(im, 0, IEJAS_IMAGE_CT_GRAY_Y);

        if bitmap.PixelFormat = ie24RGB then
        begin
          // gray scale
          RedToGrayCoef   := IEGlobalSettings().RedToGrayCoef;
          GreenToGrayCoef := IEGlobalSettings().GreenToGrayCoef;
          BlueToGrayCoef  := IEGlobalSettings().BlueToGrayCoef;
          for y := 0 to hh - 1 do
          begin
            row := Bitmap.Scanline[y];
            for x := 0 to ww - 1 do
            begin
              with row^ do
              begin
                vv := trunc((((r * RedToGrayCoef + g * GreenToGrayCoef + b * BlueToGrayCoef) div 100) / 255) * colors);
                IEJP2K_writeComponentSample(im, 0, x, y, vv);
              end;
              inc(row);
            end;
            // OnProgress
            with xProgress do
              if assigned(fOnProgress) then
                fOnProgress(Sender, trunc(per1 * y));
            if xProgress.Aborting^ then
              break;
          end;
        end
        else
        if Bitmap.PixelFormat = ie8g then
        begin
          // native 8 bit gray scale
          for y := 0 to hh - 1 do
          begin
            pb := Bitmap.Scanline[y];
            for x := 0 to ww - 1 do
            begin
              IEJP2K_writeComponentSample(im, 0, x, y, pb^);
              inc(pb);
            end;
            // OnProgress
            with xProgress do
              if assigned(fOnProgress) then
                fOnProgress(Sender, trunc(per1 * y));
            if xProgress.Aborting^ then
              break;
          end;
        end
        else
        if Bitmap.PixelFormat = ie16g then
        begin
          // native 16 bit gray scale
          for y := 0 to hh - 1 do
          begin
            pw := Bitmap.Scanline[y];
            for x := 0 to ww - 1 do
            begin
              IEJP2K_writeComponentSample(im, 0, x, y, pw^);
              inc(pw);
            end;
            // OnProgress
            with xProgress do
              if assigned(fOnProgress) then
                fOnProgress(Sender, trunc(per1 * y));
            if xProgress.Aborting^ then
              break;
          end;
        end
        else
        begin
          // black/white
          for y := 0 to hh - 1 do
          begin
            row := Bitmap.Scanline[y];
            for x := 0 to ww - 1 do
            begin
              if _GetPixelbw(pbyte(row), x) <> 0 then
                IEJP2K_writeComponentSample(im, 0, x, y, 1)
              else
                IEJP2K_writeComponentSample(im, 0, x, y, 0);
            end;
            // OnProgress
            with xProgress do
              if assigned(fOnProgress) then
                fOnProgress(Sender, trunc(per1 * y));
            if xProgress.Aborting^ then
              break;
          end;
        end;
      end;
    ioJ2000_YCbCr, // for now YCC=RGB
    ioJ2000_RGB:
      begin
        HasAlpha := Bitmap.HasAlphaChannel and not Bitmap.AlphaChannel.Full;

        if HasAlpha then
        begin
          cmptparams := IEJP2K_createComponentParametersList(4);
          IEJP2K_setComponentParameters(cmptparams, 0, 0, 0, 1, 1, ww, hh, IOParams.BitsPerSample, 0);
          IEJP2K_setComponentParameters(cmptparams, 1, 0, 0, 1, 1, ww, hh, IOParams.BitsPerSample, 0);
          IEJP2K_setComponentParameters(cmptparams, 2, 0, 0, 1, 1, ww, hh, IOParams.BitsPerSample, 0);
          IEJP2K_setComponentParameters(cmptparams, 3, 0, 0, 1, 1, ww, hh, IOParams.BitsPerSample, 0);
          im := IEJP2K_imageCreate(4, cmptparams, IEJAS_IMAGE_CS_RGB);
        end
        else
        begin
          cmptparams := IEJP2K_createComponentParametersList(3);
          IEJP2K_setComponentParameters(cmptparams, 0, 0, 0, 1, 1, ww, hh, IOParams.BitsPerSample, 0);
          IEJP2K_setComponentParameters(cmptparams, 1, 0, 0, 1, 1, ww, hh, IOParams.BitsPerSample, 0);
          IEJP2K_setComponentParameters(cmptparams, 2, 0, 0, 1, 1, ww, hh, IOParams.BitsPerSample, 0);
          im := IEJP2K_imageCreate(3, cmptparams, IEJAS_IMAGE_CS_RGB);
        end;

        IEJP2K_setComponentType(im, 0, IEJAS_IMAGE_CT_RGB_R);
        IEJP2K_setComponentType(im, 1, IEJAS_IMAGE_CT_RGB_G);
        IEJP2K_setComponentType(im, 2, IEJAS_IMAGE_CT_RGB_B);
        if HasAlpha then
          IEJP2K_setComponentType(im, 3, IEJAS_IMAGE_CT_OPACITY);

        a := nil;
        for y := 0 to hh - 1 do
        begin
          row := Bitmap.Scanline[y];
          if HasAlpha then
            a := Bitmap.AlphaChannel.ScanLine[y];
          IEJP2K_writeRowRGB8(im, ww, y, row, a, colors);
          // OnProgress
          with xProgress do
            if assigned(fOnProgress) then
              fOnProgress(Sender, trunc(per1 * y));
          if xProgress.Aborting^ then
            break;
        end;
      end;
    // Attention: YCbCr is not supported for writing. Unfortunately only Y channel is loaded.
    (*
    ioJ2000_YCbCr:
       begin
          im := jas_image_create0();
          iejas_image_setcolorspace(im, JAS_IMAGE_CS_YCBCR);
          jas_image_addcmpt(im, 0, cmptparams);
          jas_image_addcmpt(im, 1, cmptparams);
          jas_image_addcmpt(im, 2, cmptparams);
          IEJP2K_setComponentType(im, 0, (JAS_IMAGE_CT_YCBCR_Y));
          IEJP2K_setComponentType(im, 1, (JAS_IMAGE_CT_YCBCR_CB));
          IEJP2K_setComponentType(im, 2, (JAS_IMAGE_CT_YCBCR_CR));
          for y := 0 to hh-1 do begin
             row := Bitmap.Scanline[y];
             for x := 0 to ww-1 do begin
                with row^ do begin
                   yy := trunc(  0.29900 * R + 0.58700 * G + 0.11400 * B        );
                   cb := trunc( -0.16874 * R - 0.33126 * G + 0.50000 * B  + 128 );
                   Cr := trunc(  0.50000 * R - 0.41869 * G - 0.08131 * B  + 128 );
                   IEJP2K_writeComponentSample(im, 0, x, y, yy);
                   IEJP2K_writeComponentSample(im, 1, x, y, cb);
                   IEJP2K_writeComponentSample(im, 2, x, y, cr);
                end;
                inc(row);
             end;
             // OnProgress
             with xProgress do
                if assigned(fOnProgress) then
                   fOnProgress(Sender, trunc(per1*y));
             if xProgress.Aborting^ then
                break;
          end;
       end;
    //*)
  end;

  outopts := 'rate=' + IEFloatToStrS(IOParams.J2000_Rate);
  if IOParams.J2000_ScalableBy = ioJ2000_Rate then
    outopts := outopts + ' prg=lrcp'
  else
  if IOParams.J2000_ScalableBy = ioJ2000_Resolution then
    outopts := outopts + ' prg=rlcp';

  bStream := TIEBufferedWriteStream.Create(Stream, 8192);
  IEJP2K_imageEncode(im, bStream, format, outopts);
  bStream.Free();

  IEJP2K_imageDestroy(im);

  IEJP2K_destroyComponentParametersList(cmptparams);
end;


var
  iej2000Init: boolean = false;

procedure IEInitialize_iej2000();
begin
  if not iej2000Init then
  begin
    CreateYCbCrLookup();
    IEJP2K_initialize();
    iej2000Init := true;
  end;
end;


procedure IEFinalize_iej2000();
begin
  if iej2000Init then
    IEJP2K_finalize();
end;







{$else} // IEINCLUDEJPEG2000

interface

implementation

{$endif}  // IEINCLUDEJPEG2000



end.
