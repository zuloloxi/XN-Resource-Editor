unit unitDisassembler;

interface

uses
  Windows, Classes, SysUtils, unitPEFile;

type
Ti386OpType = (
  None,
  GbEb,
  GvEv,
  AlLb,
  eAXlv,
  ES,
  CS,
  DS,
  SS,
  eAX,
  eCX,
  eDX,
  eBX,
  eSP,
  eBP,
  eSI,
  eDI,
  PUSHAD,
  POPAD,
  GvMa,
  EwGw,
  FS,
  GS,
  ASize,
  OSize,
  lv,
  GvEvlv,
  lb,
  GvEvlb,
  YbDX,
  YvDX,
  DXXb,
  DXXv,

  EbGb,
  EvGv,
  Eblb,
  Eblv,
  EvLv,
  EwSw,
  Ev,
  evlb,
  eb1,
  ev1,
  ebcl,
  evcl,
  eb,
  Ep,

  GvM,
  SwEw,
  eAXCX,
  eAXDX,
  eAXBX,
  eAXSP,
  eAXBP,
  eAXSI,
  eAXDI,
  Ap,
  Fv,
  AlOb,
  eAXOv,
  ObAL,
  OvEAX,

  XbYb,
  XwYw,

  YbAl,
  YveAX,
  AlXb,
  eAXXv,
  AlYb,
  eAXYv,

  mal,
  mcl,
  mdl,
  mbl,
  mah,
  mch,
  mdh,
  mbh,

  meax,
  mecx,
  medx,
  mebx,
  mesp,
  mebp,
  mesi,
  medi,

  lw,
  GvMp,
  LwLb,
  jb,
  eaxlb,
  lbal,
  lbeax,
  jv,
  aldx,
  dxal,
  dxeax,
  GvEw,
  Escape,

  RdCd,
  RdDd,
  CdRd,
  DdRd,
  PqQd,

  EvGvLb,
  EvGvCL,

  Mp,

  PdEd,
  PqQq,
  EdPd,
  QqPq,
  GvEb,

  i2Byte,
  iCS,
  iDS,
  iSS,
  iES,
  iFS,
  iGS
  );

  TSegReg = (srDS, srES, srCS, srSS, srFS, srGS);
  TOpSize = (os32, os16);

  TDisassembler = class
  private
    FLen, fLeft: Integer;
    FP, FMem: PByte;
    FSeg: TSegReg;
    FOpSize: TOpSize;

    FRelocs: PSectionReloc;
    FRelocCount: Integer;

    FSymbols: TList;

    function DecodeOp(Optype: Ti386OpType; alreadyHasOperand: Boolean;
        Operand: byte): string;
    procedure DecodeGroup1(Optype: Ti386OpType; var st, arg: string);
    procedure DecodeGroup5(var st, arg: string);

    function SizeOfInstruction (Base: Integer): Integer;
    procedure SizeGroup1 (Optype: Ti386OpType);
    procedure SizeGroup5;

    procedure ApplyRelocs;

  public
    constructor Create (AMem: PByte; ALen: Integer; ARelocs: PSectionReloc; ARelocCount: Integer; ASymbols: TList);
    destructor Destroy; override;

    function Disassemble(Base: Integer; var op, param: string): Integer; overload;
    procedure Disassemble (s: TStream); overload;
    procedure GetLineMap (lineMap: TList);

    property Memory: PByte read FMem;
    property MemorySize: Integer read FLen;
  end;


implementation

type
  Ti386OpCode = packed record
    op: byte;
    txt: string [13];
    Optype: Ti386OpType;
  end;

var
  gi386OpCodes: array [0..255] of Ti386OpCode = (
    (op:$00; txt:'add';    Optype:EbGb),
    (op:$01; txt:'add';    Optype:EvGv),
    (op:$02; txt:'add';    Optype:GbEb),
    (op:$03; txt:'add';    Optype:GvEv),
    (op:$04; txt:'add';    Optype:AlLb),
    (op:$05; txt:'add';    Optype:eAXLv),
    (op:$06; txt:'push';   Optype:ES),
    (op:$07; txt:'pop';    Optype:ES),

    (op:$08; txt:'or';     Optype:EbGb),
    (op:$09; txt:'or';     Optype:EvGv),
    (op:$0A; txt:'or';     Optype:GbEb),
    (op:$0B; txt:'or';     Optype:GvEv),
    (op:$0C; txt:'or';     Optype:AlLb),
    (op:$0D; txt:'or';     Optype:eAXLv),
    (op:$0E; txt:'push';   Optype:CS),
    (op:$0F; txt:'';       Optype:i2Byte),  // 2 byte opcode indicator

    (op:$10; txt:'ADC';    Optype:EbGb),
    (op:$11; txt:'ADC';    Optype:EvGv),
    (op:$12; txt:'ADC';    Optype:GbEb),
    (op:$13; txt:'ADC';    Optype:GvEv),
    (op:$14; txt:'ADC';    Optype:AlLb),
    (op:$15; txt:'ADC';    Optype:eAXLv),
    (op:$16; txt:'PUSH';   Optype:SS),
    (op:$17; txt:'POP';    Optype:SS),

    (op:$18; txt:'SBB';    Optype:EbGb),
    (op:$19; txt:'SBB';    Optype:EvGv),
    (op:$1A; txt:'SBB';    Optype:GbEb),
    (op:$1B; txt:'SBB';    Optype:GvEv),
    (op:$1C; txt:'SBB';    Optype:AlLb),
    (op:$1D; txt:'SBB';    Optype:eAXLv),
    (op:$1E; txt:'PUSH';   Optype:DS),
    (op:$1F; txt:'POP';    Optype:DS),

    (op:$20; txt:'AND';    Optype:EbGb),
    (op:$21; txt:'AND';    Optype:EvGv),
    (op:$22; txt:'AND';    Optype:GbEb),
    (op:$23; txt:'AND';    Optype:GvEv),
    (op:$24; txt:'AND';    Optype:AlLb),
    (op:$25; txt:'AND';    Optype:eAXLv),
    (op:$26; txt:'SEG';    Optype:iES),
    (op:$27; txt:'DAA';    Optype:None),

    (op:$28; txt:'SUB';    Optype:EbGb),
    (op:$29; txt:'SUB';    Optype:EvGv),
    (op:$2A; txt:'SUB';    Optype:GbEb),
    (op:$2B; txt:'SUB';    Optype:GvEv),
    (op:$2C; txt:'SUB';    Optype:AlLb),
    (op:$2D; txt:'SUB';    Optype:eAXLv),
    (op:$2E; txt:'SEG';    Optype:iCS),
    (op:$2F; txt:'DAS';    Optype:None),

    (op:$30; txt:'xor';    Optype:EbGb),
    (op:$31; txt:'xor';    Optype:EvGv),
    (op:$32; txt:'xor';    Optype:GbEb),
    (op:$33; txt:'xor';    Optype:GvEv),
    (op:$34; txt:'xor';    Optype:AlLb),
    (op:$35; txt:'xor';    Optype:eAXLv),
    (op:$36; txt:'seg';    Optype:iSS),
    (op:$37; txt:'aaa';    Optype:None),

    (op:$38; txt:'cmp';    Optype:EbGb),
    (op:$39; txt:'cmp';    Optype:EvGv),
    (op:$3A; txt:'cmp';    Optype:GbEb),
    (op:$3B; txt:'cmp';    Optype:GvEv),
    (op:$3C; txt:'cmp';    Optype:AlLb),
    (op:$3D; txt:'cmp';    Optype:eAXLv),
    (op:$3E; txt:'seg';    Optype:iDS),
    (op:$3F; txt:'aas';    Optype:None),

    (op:$40; txt:'INC';    Optype:eAX),
    (op:$41; txt:'INC';    Optype:eCX),
    (op:$42; txt:'INC';    Optype:eDX),
    (op:$43; txt:'INC';    Optype:eBX),
    (op:$44; txt:'INC';    Optype:eSP),
    (op:$45; txt:'INC';    Optype:eBP),
    (op:$46; txt:'INC';    Optype:eSI),
    (op:$47; txt:'INC';    Optype:eDI),

    (op:$48; txt:'DEC';    Optype:eAX),
    (op:$49; txt:'DEC';    Optype:eCX),
    (op:$4A; txt:'DEC';    Optype:eDX),
    (op:$4B; txt:'DEC';    Optype:eBX),
    (op:$4C; txt:'DEC';    Optype:eSP),
    (op:$4D; txt:'DEC';    Optype:eBP),
    (op:$4E; txt:'DEC';    Optype:eSI),
    (op:$4F; txt:'DEC';    Optype:eDI),

    (op:$50; txt:'push';    Optype:eAX),
    (op:$51; txt:'push';    Optype:eCX),
    (op:$52; txt:'push';    Optype:eDX),
    (op:$53; txt:'push';    Optype:eBX),
    (op:$54; txt:'push';    Optype:eSP),
    (op:$55; txt:'push';    Optype:eBP),
    (op:$56; txt:'push';    Optype:eSI),
    (op:$57; txt:'push';    Optype:eDI),

    (op:$58; txt:'pop';    Optype:eAX),
    (op:$59; txt:'pop';    Optype:eCX),
    (op:$5A; txt:'pop';    Optype:eDX),
    (op:$5B; txt:'pop';    Optype:eBX),
    (op:$5C; txt:'pop';    Optype:eSP),
    (op:$5D; txt:'pop';    Optype:eBP),
    (op:$5E; txt:'pop';    Optype:eSI),
    (op:$5F; txt:'pop';    Optype:eDI),

    (op:$60; txt:'PUSHA';  Optype:PUSHAD),
    (op:$61; txt:'POPA';   Optype:POPAD),
    (op:$62; txt:'BOUND';  Optype:GvMa),
    (op:$63; txt:'ARPL';   Optype:EwGw),
    (op:$64; txt:'SEG';    Optype:iFS),
    (op:$65; txt:'SEG';    Optype:iGS),
    (op:$66; txt:'Operand';Optype:OSize),
    (op:$67; txt:'Operand';Optype:ASize),

    (op:$68; txt:'push';   Optype:lv),
    (op:$69; txt:'imul';   Optype:GvEvlv),
    (op:$6A; txt:'push';   Optype:lb),
    (op:$6B; txt:'imul';   Optype:GvEvlb),
    (op:$6C; txt:'insb';   Optype:YbDX),
    (op:$6D; txt:'insd';  Optype:YvDX),
    (op:$6E; txt:'outsb';  Optype:DxXb),
    (op:$6F; txt:'outsd'; Optype:DxXv),

    (op:$70; txt:'jo'; Optype:jb),
    (op:$71; txt:'jno'; Optype:jb),
    (op:$72; txt:'jb'; Optype:jb),
    (op:$73; txt:'jnb'; Optype:jb),
    (op:$74; txt:'je'; Optype:jb),
    (op:$75; txt:'jne'; Optype:jb),
    (op:$76; txt:'jbe'; Optype:jb),
    (op:$77; txt:'jnbe'; Optype:jb),

    (op:$78; txt:'js'; Optype:jb),
    (op:$79; txt:'jns'; Optype:jb),
    (op:$7A; txt:'jp'; Optype:jb),
    (op:$7B; txt:'jnp'; Optype:jb),
    (op:$7C; txt:'jl'; Optype:jb),
    (op:$7D; txt:'jnl'; Optype:jb),
    (op:$7E; txt:'jle'; Optype:jb),
    (op:$7F; txt:'jnle'; Optype:jb),

    (op:$80; txt:'~1'; Optype:eblb),
    (op:$81; txt:'~1'; Optype:evlv),
    (op:$82; txt:'~1'; Optype:evlb),
    (op:$83; txt:'~1'; Optype:eblv),
    (op:$84; txt:'TEST'; Optype:Ebgb),
    (op:$85; txt:'TEST'; Optype:Evgv),
    (op:$86; txt:'XCHG'; Optype:Ebgb),
    (op:$87; txt:'XCHG'; Optype:Evgv),

    (op:$88; txt:'MOV'; Optype:Ebgb),
    (op:$89; txt:'MOV'; Optype:Evgv),
    (op:$8A; txt:'MOV'; Optype:GbEb),
    (op:$8B; txt:'MOV'; Optype:GvEv),
    (op:$8C; txt:'MOV'; Optype:EwSw),
    (op:$8D; txt:'LEA'; Optype:GvM),
    (op:$8E; txt:'MOV'; Optype:SwEw),
    (op:$8F; txt:'POP'; Optype:Ev),

    (op:$90; txt:'NOP'; Optype:None),
    (op:$91; txt:'XCHG'; Optype:eAXCX),
    (op:$92; txt:'XCHG'; Optype:eAXDX),
    (op:$93; txt:'XCHG'; Optype:eAXBX),
    (op:$94; txt:'XCHG'; Optype:eAXSP),
    (op:$95; txt:'XCHG'; Optype:eAXBP),
    (op:$96; txt:'XCHG'; Optype:eAXSI),
    (op:$97; txt:'XCHG'; Optype:eAXDI),

    (op:$98; txt:'CBW'; Optype:None),
    (op:$99; txt:'CWD'; Optype:None),
    (op:$9A; txt:'CALL'; Optype:Ap),
    (op:$9B; txt:'WAIT'; Optype:None),
    (op:$9C; txt:'PUSHF'; Optype:Fv),
    (op:$9D; txt:'POPF'; Optype:Fv),
    (op:$9E; txt:'SAHF'; Optype:None),
    (op:$9F; txt:'LAHF'; Optype:None),

    (op:$A0; txt:'MOV'; Optype:ALOb),
    (op:$A1; txt:'MOV'; Optype:eAXOv),
    (op:$A2; txt:'MOV'; Optype:ObAL),
    (op:$A3; txt:'MOV'; Optype:OvEAX),
    (op:$A4; txt:'MOVSB'; Optype:XbYb),
    (op:$A5; txt:'MOVSW'; Optype:XwYw),
    (op:$A6; txt:'CMPSB'; Optype:XbYb),
    (op:$A7; txt:'CMPSW'; Optype:XwYw),

    (op:$A8; txt:'TEST'; Optype:Allb),
    (op:$A9; txt:'TEST'; Optype:eAXLv),
    (op:$AA; txt:'STOSB'; Optype:YbAl),
    (op:$AB; txt:'STOSW'; Optype:YveAX),
    (op:$AC; txt:'LODSB'; Optype:AlXb),
    (op:$AD; txt:'LODWSW'; Optype:eAXXv),
    (op:$AE; txt:'SCASB'; Optype:AlYb),
    (op:$AF; txt:'SCASW'; Optype:eAXYv),

    (op:$B0; txt:'MOV'; Optype:mAl),
    (op:$B1; txt:'MOV'; Optype:mCl),
    (op:$B2; txt:'MOV'; Optype:mDl),
    (op:$B3; txt:'MOV'; Optype:mBl),
    (op:$B4; txt:'MOV'; Optype:mAh),
    (op:$B5; txt:'MOV'; Optype:mCh),
    (op:$B6; txt:'MOV'; Optype:mDh),
    (op:$B7; txt:'MOV'; Optype:mBh),

    (op:$B8; txt:'MOV'; Optype:meax),
    (op:$B9; txt:'MOV'; Optype:mecx),
    (op:$BA; txt:'MOV'; Optype:medx),
    (op:$BB; txt:'MOV'; Optype:mebx),
    (op:$BC; txt:'MOV'; Optype:mesp),
    (op:$BD; txt:'MOV'; Optype:mebp),
    (op:$BE; txt:'MOV'; Optype:mesi),
    (op:$BF; txt:'MOV'; Optype:medi),

    (op:$C0; txt:'~2'; Optype:eblb),
    (op:$C1; txt:'~2'; Optype:evlb),
    (op:$C2; txt:'RET'; Optype:lw),   { near ret }
    (op:$C3; txt:'RET'; Optype:None),
    (op:$C4; txt:'LES'; Optype:GvMp),
    (op:$C5; txt:'LDS'; Optype:GvMp),
    (op:$C6; txt:'MOV'; Optype:Eblb),
    (op:$C7; txt:'MOV'; Optype:EvLv),

    (op:$C8; txt:'ENTER'; Optype:LwLb),
    (op:$C9; txt:'LEAVE'; Optype:None),
    (op:$CA; txt:'RET'; Optype:lw),  { far ret }
    (op:$CB; txt:'RET'; Optype:None),
    (op:$CC; txt:'INT 3'; Optype:None),
    (op:$CD; txt:'INT'; Optype:lb),
    (op:$CE; txt:'INTO'; Optype:None),
    (op:$CF; txt:'IRET'; Optype:None),

    (op:$D0; txt:'~2'; Optype:eb1),
    (op:$D1; txt:'~2'; Optype:ev1),
    (op:$D2; txt:'~2'; Optype:ebcl),
    (op:$D3; txt:'~2'; Optype:evcl),
    (op:$D4; txt:'AAM'; Optype:None),
    (op:$D5; txt:'AAD'; Optype:None),
    (op:$D6; txt:''; Optype:None),
    (op:$D7; txt:'XLAT'; Optype:None),

    (op:$D8; txt:''; Optype:Escape),
    (op:$D9; txt:''; Optype:Escape),
    (op:$DA; txt:''; Optype:Escape),
    (op:$DB; txt:''; Optype:Escape),
    (op:$DC; txt:''; Optype:Escape),
    (op:$DD; txt:''; Optype:Escape),
    (op:$DE; txt:''; Optype:Escape),
    (op:$DF; txt:''; Optype:Escape),

    (op:$E0; txt:'LOOPN'; Optype:Jb),
    (op:$E1; txt:'LOOPE'; Optype:Jb),
    (op:$E2; txt:'LOOP'; Optype:Jb),
    (op:$E3; txt:'JCXZ'; Optype:Jb),
    (op:$E4; txt:'IN'; Optype:Allb),
    (op:$E5; txt:'IN'; Optype:eaxlb),
    (op:$E6; txt:'OUT'; Optype:lbal),
    (op:$E7; txt:'LOOPN'; Optype:lbeax),

    (op:$E8; txt:'CALL'; Optype:Jv),
    (op:$E9; txt:'JMP'; Optype:Jv),
    (op:$EA; txt:'JMP'; Optype:Ap),
    (op:$EB; txt:'JMP'; Optype:Jb),
    (op:$EC; txt:'IN'; Optype:aldx),
    (op:$ED; txt:'IN'; Optype:eaxdx),
    (op:$EE; txt:'OUT'; Optype:dxal),
    (op:$EF; txt:'OUT'; Optype:dxeax),

    (op:$F0; txt:'LOCK'; Optype:none), { prefix }
    (op:$F1; txt:''; Optype:none),
    (op:$F2; txt:'REPNE'; Optype:none), { prefix }
    (op:$F3; txt:'REP'; Optype:none), { prefix }
    (op:$F4; txt:'HLT'; Optype:none),
    (op:$F5; txt:'CMC'; Optype:none),
    (op:$F6; txt:'~3'; Optype:eb),
    (op:$F7; txt:'~3'; Optype:ev),

    (op:$F8; txt:'CLC'; Optype:none),
    (op:$F9; txt:'STC'; Optype:none),
    (op:$FA; txt:'CLI'; Optype:none),
    (op:$FB; txt:'STI'; Optype:none),
    (op:$FC; txt:'CLD'; Optype:none),
    (op:$FD; txt:'STD'; Optype:none),
    (op:$FE; txt:'~4'; Optype:none),
    (op:$FF; txt:'~5'; Optype:none)
  );

  g2Bytei386OpCodes: array [0..255] of Ti386OpCode = (
    (op:$00; txt:'~6'; Optype:none),
    (op:$01; txt:''; Optype:none),
    (op:$02; txt:'LAR'; Optype:GvEw),
    (op:$03; txt:'LSL'; Optype:GvEw),
    (op:$04; txt:''; Optype:none),
    (op:$05; txt:''; Optype:none),
    (op:$06; txt:'CLTS'; Optype:none),
    (op:$07; txt:''; Optype:none),

    (op:$08; txt:'INVD'; Optype:none),
    (op:$09; txt:'WBINVD'; Optype:none),
    (op:$0a; txt:''; Optype:none),
    (op:$0b; txt:'UD2'; Optype:none),
    (op:$0c; txt:''; Optype:none),
    (op:$0d; txt:''; Optype:none),
    (op:$0e; txt:''; Optype:none),
    (op:$0f; txt:''; Optype:none),

    (op:$10; txt:''; Optype:none),
    (op:$11; txt:''; Optype:none),
    (op:$12; txt:''; Optype:none),
    (op:$13; txt:''; Optype:none),
    (op:$14; txt:''; Optype:none),
    (op:$15; txt:''; Optype:none),
    (op:$16; txt:''; Optype:none),
    (op:$17; txt:''; Optype:none),

    (op:$18; txt:''; Optype:none),
    (op:$19; txt:''; Optype:none),
    (op:$1a; txt:''; Optype:none),
    (op:$1b; txt:''; Optype:none),
    (op:$1c; txt:''; Optype:none),
    (op:$1d; txt:''; Optype:none),
    (op:$1e; txt:''; Optype:none),
    (op:$1f; txt:''; Optype:none),

    (op:$20; txt:'MOV'; Optype:RdCd),
    (op:$21; txt:'MOV'; Optype:RdDd),
    (op:$22; txt:'MOV'; Optype:CdRd),
    (op:$23; txt:'MOV'; Optype:DdRd),
    (op:$24; txt:''; Optype:none),
    (op:$25; txt:''; Optype:none),
    (op:$26; txt:''; Optype:none),
    (op:$27; txt:''; Optype:none),

    (op:$28; txt:''; Optype:none),
    (op:$29; txt:''; Optype:none),
    (op:$2a; txt:''; Optype:none),
    (op:$2b; txt:''; Optype:none),
    (op:$2c; txt:''; Optype:none),
    (op:$2d; txt:''; Optype:none),
    (op:$2e; txt:''; Optype:none),
    (op:$2f; txt:''; Optype:none),

    (op:$30; txt:'WRMSR'; Optype:none),
    (op:$31; txt:'RDTSC'; Optype:none),
    (op:$32; txt:'RDMSR'; Optype:none),
    (op:$33; txt:'RDPMC'; Optype:none),
    (op:$34; txt:''; Optype:none),
    (op:$35; txt:''; Optype:none),
    (op:$36; txt:''; Optype:none),
    (op:$37; txt:''; Optype:none),

    (op:$38; txt:''; Optype:none),
    (op:$39; txt:''; Optype:none),
    (op:$3a; txt:''; Optype:none),
    (op:$3b; txt:''; Optype:none),
    (op:$3c; txt:''; Optype:none),
    (op:$3d; txt:''; Optype:none),
    (op:$3e; txt:''; Optype:none),
    (op:$3f; txt:''; Optype:none),

    (op:$40; txt:'CMOVO'; Optype:GvEv),
    (op:$41; txt:'CMOVNO'; Optype:GvEv),
    (op:$42; txt:'CMOVB'; Optype:GvEv),
    (op:$43; txt:'CMOVAE'; Optype:GvEv),
    (op:$44; txt:'CMOVE'; Optype:GvEv),
    (op:$45; txt:'CMOVNE'; Optype:GvEv),
    (op:$46; txt:'CMOVBE'; Optype:GvEv),
    (op:$47; txt:'CMOVA'; Optype:GvEv),

    (op:$48; txt:'CMOVS'; Optype:GvEv),
    (op:$49; txt:'CMOVNS'; Optype:GvEv),
    (op:$4a; txt:'CMOVP'; Optype:GvEv),
    (op:$4b; txt:'CMOVNP'; Optype:GvEv),
    (op:$4c; txt:'CMOVL'; Optype:GvEv),
    (op:$4d; txt:'CMOVGE'; Optype:GvEv),
    (op:$4e; txt:'CMOVLE'; Optype:GvEv),
    (op:$4f; txt:'CMOVG'; Optype:GvEv),

    (op:$50; txt:''; Optype:none),
    (op:$51; txt:''; Optype:none),
    (op:$52; txt:''; Optype:none),
    (op:$53; txt:''; Optype:none),
    (op:$54; txt:''; Optype:none),
    (op:$55; txt:''; Optype:none),
    (op:$56; txt:''; Optype:none),
    (op:$57; txt:''; Optype:none),

    (op:$58; txt:''; Optype:none),
    (op:$59; txt:''; Optype:none),
    (op:$5a; txt:''; Optype:none),
    (op:$5b; txt:''; Optype:none),
    (op:$5c; txt:''; Optype:none),
    (op:$5d; txt:''; Optype:none),
    (op:$5e; txt:''; Optype:none),
    (op:$5f; txt:''; Optype:none),

    (op:$60; txt:'PUNPCKLWB'; Optype:PqQd),
    (op:$61; txt:'PUNPCKLWD'; Optype:PqQd),
    (op:$62; txt:'PUNOCKLDQ'; Optype:PqQd),
    (op:$63; txt:'PACKUSDW'; Optype:PqQd),
    (op:$64; txt:'PCMPGTB'; Optype:PqQd),
    (op:$65; txt:'PCMPGTW'; Optype:PqQd),
    (op:$66; txt:'PCMPGTD'; Optype:PqQd),
    (op:$67; txt:'PACKSSWB'; Optype:PqQd),

    (op:$68; txt:'PUNPCKHWB'; Optype:PqQd),
    (op:$69; txt:'PUNPCKHWD'; Optype:PqQd),
    (op:$6a; txt:'PUNPCKHWQ'; Optype:PqQd),
    (op:$6b; txt:'PACKSSDW'; Optype:PqQd),
    (op:$6c; txt:''; Optype:none),
    (op:$6d; txt:''; Optype:none),
    (op:$6e; txt:'MOVD'; Optype:PdEd),
    (op:$6f; txt:'MOVQ'; Optype:PqQq),

    (op:$70; txt:''; Optype:none),
    (op:$71; txt:'PSHIMW'; Optype:none),
    (op:$72; txt:'PSHIMD'; Optype:none),
    (op:$73; txt:'PSHIMQ'; Optype:none),
    (op:$74; txt:'PCMPEQB'; Optype:PqQd),
    (op:$75; txt:'PCMPEQW'; Optype:PqQd),
    (op:$76; txt:'PCMPEQD'; Optype:PqQd),
    (op:$77; txt:'EMMS'; Optype:none),

    (op:$78; txt:''; Optype:none),
    (op:$79; txt:''; Optype:none),
    (op:$7a; txt:''; Optype:none),
    (op:$7b; txt:''; Optype:none),
    (op:$7c; txt:''; Optype:none),
    (op:$7d; txt:''; Optype:none),
    (op:$7e; txt:'MOVD'; Optype:EdPd),
    (op:$7f; txt:'MOVQ'; Optype:QqPq),

    (op:$80; txt:'jo'; Optype:jv),
    (op:$81; txt:'jno'; Optype:jv),
    (op:$82; txt:'jb'; Optype:jv),
    (op:$83; txt:'jnb'; Optype:jv),
    (op:$84; txt:'je'; Optype:jv),
    (op:$85; txt:'jne'; Optype:jv),
    (op:$86; txt:'jbe'; Optype:jv),
    (op:$87; txt:'jnbe'; Optype:jv),

    (op:$88; txt:'js'; Optype:jv),
    (op:$89; txt:'jns'; Optype:jv),
    (op:$8A; txt:'jp'; Optype:jv),
    (op:$8B; txt:'jnp'; Optype:jv),
    (op:$8C; txt:'jl'; Optype:jv),
    (op:$8D; txt:'jnl'; Optype:jv),
    (op:$8E; txt:'jle'; Optype:jv),
    (op:$8F; txt:'jnle'; Optype:jv),

    (op:$90; txt:'seto'; Optype:Eb),
    (op:$91; txt:'setno'; Optype:Eb),
    (op:$92; txt:'setb'; Optype:Eb),
    (op:$93; txt:'setnb'; Optype:Eb),
    (op:$94; txt:'sete'; Optype:Eb),
    (op:$95; txt:'setne'; Optype:Eb),
    (op:$96; txt:'setbe'; Optype:Eb),
    (op:$97; txt:'setnbe'; Optype:Eb),

    (op:$98; txt:'SETS'; Optype:Eb),
    (op:$99; txt:'SETNS'; Optype:Eb),
    (op:$9a; txt:'SETP'; Optype:Eb),
    (op:$9b; txt:'SETNP'; Optype:Eb),
    (op:$9c; txt:'SETL'; Optype:Eb),
    (op:$9d; txt:'SETNL'; Optype:Eb),
    (op:$9e; txt:'SETLE'; Optype:Eb),
    (op:$9f; txt:'SETNLE'; Optype:Eb),

    (op:$a0; txt:'PUSH'; Optype:FS),
    (op:$a1; txt:'POP'; Optype:FS),
    (op:$a2; txt:'CPUID'; Optype:none),
    (op:$a3; txt:'BT'; Optype:EvGv),
    (op:$a4; txt:'SHLD'; Optype:EvGvLb),
    (op:$a5; txt:'SHLD'; Optype:EvGvCL),
    (op:$a6; txt:''; Optype:none),
    (op:$a7; txt:''; Optype:none),

    (op:$a8; txt:'PUSH'; Optype:GS),
    (op:$a9; txt:'POP'; Optype:GS),
    (op:$aa; txt:'RSM'; Optype:none),
    (op:$ab; txt:'BTS'; Optype:EvGv),
    (op:$ac; txt:'SHRD'; Optype:EvGvLb),
    (op:$ad; txt:'SHRD'; Optype:EvGvCL),
    (op:$ae; txt:''; Optype:none),
    (op:$af; txt:'IMUL'; Optype:GvEv),

    (op:$b0; txt:'CMPXCHG'; Optype:EbGb),
    (op:$b1; txt:'CMPXCHG'; Optype:EvGv),
    (op:$b2; txt:'LSS'; Optype:Mp),
    (op:$b3; txt:'BTR'; Optype:EvGv),
    (op:$b4; txt:'LFS'; Optype:none),
    (op:$b5; txt:'LGS'; Optype:none),
    (op:$b6; txt:'MOVZX'; Optype:GbEb),
    (op:$b7; txt:'MOVZX'; Optype:GvEw),

    (op:$b8; txt:''; Optype:none),
    (op:$b9; txt:''; Optype:none),
    (op:$ba; txt:'~8'; Optype:none),
    (op:$bb; txt:'BTC'; Optype:EvGv),
    (op:$bc; txt:'BSF'; Optype:EvGv),
    (op:$bd; txt:'BSR'; Optype:EvGv),
    (op:$be; txt:'MOVSX'; Optype:GvEb),
    (op:$bf; txt:'MOVSX'; Optype:GvEw),

    (op:$c0; txt:'XADD'; Optype:EbGb),
    (op:$c1; txt:'XADD'; Optype:EvGv),
    (op:$c2; txt:''; Optype:none),
    (op:$c3; txt:''; Optype:none),
    (op:$c4; txt:''; Optype:none),
    (op:$c5; txt:''; Optype:none),
    (op:$c6; txt:''; Optype:none),
    (op:$c7; txt:'~9'; Optype:none),

    (op:$c8; txt:'BSWAP'; Optype:EAX),
    (op:$c9; txt:'BSWAP'; Optype:ECX),
    (op:$ca; txt:'BSWAP'; Optype:EDX),
    (op:$cb; txt:'BSWAP'; Optype:EBX),
    (op:$cc; txt:'BSWAP'; Optype:ESP),
    (op:$cd; txt:'BSWAP'; Optype:EBP),
    (op:$ce; txt:'BSWAP'; Optype:ESI),
    (op:$cf; txt:'BSWAP'; Optype:EDI),

    (op:$d0; txt:''; Optype:none),
    (op:$d1; txt:'PSRLW'; Optype:PqQd),
    (op:$d2; txt:'PSRLD'; Optype:PqQd),
    (op:$d3; txt:'PSRLQ'; Optype:PqQd),
    (op:$d4; txt:''; Optype:none),
    (op:$d5; txt:'PMULLW'; Optype:PqQd),
    (op:$d6; txt:''; Optype:none),
    (op:$d7; txt:''; Optype:none),

    (op:$d8; txt:''; Optype:none),
    (op:$d9; txt:''; Optype:none),
    (op:$da; txt:''; Optype:none),
    (op:$db; txt:''; Optype:none),
    (op:$dc; txt:''; Optype:none),
    (op:$dd; txt:''; Optype:none),
    (op:$de; txt:''; Optype:none),
    (op:$df; txt:''; Optype:none),

    (op:$e0; txt:''; Optype:none),
    (op:$e1; txt:'PSRAW'; Optype:PqQd),
    (op:$e2; txt:'PSRAD'; Optype:PqQd),
    (op:$e3; txt:''; Optype:none),
    (op:$e4; txt:''; Optype:none),
    (op:$e5; txt:'PMULAW'; Optype:PqQd),
    (op:$e6; txt:''; Optype:none),
    (op:$e7; txt:''; Optype:none),

    (op:$e8; txt:'PSUBUSB'; Optype:PqQq),
    (op:$e9; txt:'PSUBUSW'; Optype:PqQq),
    (op:$ea; txt:''; Optype:none),
    (op:$eb; txt:'PAND'; Optype:PqQq),
    (op:$ec; txt:'PADDUSB'; Optype:PqQq),
    (op:$ed; txt:'PADDUSW'; Optype:PqQq),
    (op:$ee; txt:''; Optype:none),
    (op:$ef; txt:'PANDN'; Optype:PqQq),

    (op:$f0; txt:''; Optype:none),
    (op:$f1; txt:'PSLLW'; Optype:PqQd),
    (op:$f2; txt:'PSLLD'; Optype:PqQd),
    (op:$f3; txt:'PSLLQ'; Optype:PqQd),
    (op:$f4; txt:''; Optype:none),
    (op:$f5; txt:'PMADDWD'; Optype:PqQd),
    (op:$f6; txt:''; Optype:none),
    (op:$f7; txt:''; Optype:none),

    (op:$f8; txt:'PSUBSB'; Optype:PqQq),
    (op:$f9; txt:'PSUBSW'; Optype:PqQq),
    (op:$fa; txt:''; Optype:none),
    (op:$fb; txt:'POR'; Optype:PqQq),
    (op:$fc; txt:'PADDSB'; Optype:PqQq),
    (op:$fd; txt:'PADDSW'; Optype:PqQq),
    (op:$fe; txt:''; Optype:none),
    (op:$ff; txt:'PXOR'; Optype:PqQq)

  );
type
  TRegType = (r8, r16, r32, mm);

var
  gRegisters: array [TRegType, 0..7] of string [3] = (
    ('al', 'cl', 'dl', 'bl', 'ah', 'ch', 'dh', 'bh'),
    ('ax', 'cx', 'dx', 'bx', 'sp', 'bp', 'si', 'di'),
    ('eax', 'ecx', 'edx', 'ebx', 'esp', 'ebp', 'esi', 'edi'),
    ('mm0', 'mm1', 'mm2', 'mm3', 'mm4', 'mm5', 'mm6', 'mm7'));


function TDisassembler.DecodeOp (Optype: Ti386OpType; alreadyHasOperand: Boolean; Operand: byte): string;
var
  i: Integer;

  procedure CheckLen (requiredLen: Integer);
  begin
    if fLeft < requiredLen then
      raise Exception.Create ('End of data');

  end;

  function GetByte: byte;
  begin
    checkLen (sizeof (byte));
    Result := FP^;
    Inc(FP, sizeof (byte));
    Dec(fLeft, sizeof (byte))
  end;

  function GetWord: Word;
  begin
    checkLen (sizeof (Word));
    Result := PWord (FP)^;
    Inc(FP, sizeof (Word));
    Dec(fLeft, sizeof (Word))
  end;

  function GetDWord: DWord;
  begin
    checkLen (sizeof (DWord));
    Result := PDWord (FP)^;
    Inc(FP, sizeof (DWord));
    Dec(fLeft, sizeof (DWord))
  end;

  function GetWordOrDWORD: DWord;
  begin
    if FOpSize = os16 then
      Result := GetWord
    else
      Result := GetDWORD
  end;


  function FormatByte (b: byte; const prefix, suffix: string): string;
  begin
    Result := prefix  + IntToHex (b, 2 * sizeof (byte)) + suffix;
  end;

  function FormatDWORD (d: DWORD; const prefix, suffix: string): string;
  begin
    Result := prefix  + IntToHex (d, 2 * sizeof (DWORD)) + suffix;
  end;

  function FormatWord (w: Word; const prefix, suffix: string): string;
  begin
    Result := prefix  + IntToHex (w, 2 * sizeof (WORD)) + suffix;
  end;

  // method :: 0 = r8, 1 = r16, 2 = r32, 3 = mm
  function DecodeSIB (sib: byte; regType: TRegType): string;
  var
    ss, idx, md: byte;
  begin
    ss := sib shr 6;
    idx := (sib shr 3) and 7;
    md := sib and 7;


    if md <> 4 then
    begin
      Result := string (gRegisters  [regType, md]);
      Result := result + '+' + string (gRegisters [r32, idx]);
      case ss of
        1: Result := result + '*2';
        2: Result := result + '*4';
        3: Result := result + '*8';
      end
    end
    else
      Result := string (gRegisters  [regType, idx]);
  end;

  function ptrType (reg: TRegType): string;
  begin
    if FOpSize = os16 then
      reg := r16;

    Result := '';
    case reg of
      r8: Result := 'byte ptr ';
      r16: Result := 'Word ptr ';
      r32: Result := 'dWord ptr '
    end
  end;

  procedure EmitMODRM (Operand: byte; rt: TRegType; hasReg: Boolean);
  var
    _mod: byte;
    _rm: byte;
    sib: byte;
    reg: byte;
    dw: DWORD;

    function SegMod: string;
    begin
      Result := '';
      case FSeg of
        srDS:;
        srES: Result := 'es:';
        srCS: Result := 'cs:';
        srSS: Result := 'ss:';
        srFS: Result := 'fs:';
        srGS: Result := 'gs:';
      end;
    end;

  begin
   _rm := Operand and 7;
   _mod := Operand shr 6;
   reg := Operand shr 3 and 7;
   Result := '';

   if hasReg then
     Result := string (gRegisters [rt, reg] + ',');

   case _mod of
     0: case _rm of
           4: // [--][--]
             begin
               sib := GetByte;
               Result := result+ '[' + SegMod + DecodeSib (sib, r32) + (* '+' + gRegisters [r32,reg] + *) ']'
             end;
           5 :
             begin
               dw := GetDWORD;
               Result := FormatDWORD (dw, ptrType (rt) + '[',']');
             end
           else
             Result := result + '[' + SegMod + string (gRegisters [r32,_rm]) + ']';
         end;

     1 :
       begin
         case _rm of
           4: // disp8 [--][--]
             begin
               sib := GetByte;
               Result := result+ 'dWord ptr [' + SegMod + DecodeSib (sib, r32) + '+' + IntToHex (GetByte, 2) + ']'
             end;
           else
             begin
               sib := GetByte;
               if sib = 0 then
                 Result := result + '[' + SegMod + string (gRegisters [r32, _rm]) + ']'
               else
                 Result := result + '[' + SegMod + string (gRegisters [r32, _rm]) + '+' + IntToHex (sib, 2) + ']';
             end
         end
       end;

     2 :
       begin
         case _rm of
           4: // disp32 [--][--]
             begin
               sib := GetByte;
               Result := result+ 'dWord ptr [' + DecodeSib (sib, r32) + '+' + IntToHex (GetDWORD, 8) + ']'
             end;
           else
             begin
               dw := GetDWORD;
               Result := result + '[' + string (gRegisters [r32, _rm]) + '+' + IntToHex (dw, 8) + ']';
             end
         end
       end;

     3 :
       begin
         if hasReg then
           Result := string (gRegisters [rt, reg]) + ',';
         Result := result + string (gRegisters [rt, _rm])
       end;
   end
  end;

  function GetOperand: byte;
  begin
    if not alreadyHasOperand then
      Operand := GetByte;
    Result := Operand;
  end;

begin
  Result := '';
  case Optype of
    None: ;
    EbGb:
      begin
        EmitMODRM (GetOperand, r8, false);
        Result := result + ',' + string (gRegisters [r8, (Operand shr 3) and 7]);
      end;

    EvGv:
      begin
        EmitMODRM (GetOperand, r32, false);
        Result := result + ',' + string (gRegisters [r32, (Operand shr 3) and 7]);
      end;
    GbEb:
      begin
        EmitMODRM (GetOperand, r8, true);
//        Result := gRegisters [r8, (Operand shr 3) and 7] + ',' + result;
      end;
    GvEv:
      begin
        EmitMODRM (GetOperand, r32, true);
//          Result := gRegisters [r32, (Operand shr 3) and 7] + ',' + result;
      end;
    GvEb :
      begin
        EmitMODRM (GetOperand, r8, true);
//          Result := gRegisters [r32, (Operand shr 3) and 7] + ',' + result;
      end;

    Eblb:
      begin
        EmitMODRM (GetOperand, r8, false);
        Result := result + ',' + IntToHex (GetByte, 2);
      end;
    Eblv:
      begin
        EmitMODRM (GetOperand, r32, false);
        Result := result + ',' + IntToHex (GetByte, 2);
      end;
    Evlv:
      begin
        EmitMODRM (GetOperand, r32, false);
        Result := result + ',' + IntToHex (GetWordOrDWORD, 8);
      end;

    AlLb: Result := FormatByte (GetByte, 'AL, ', '');
    eAXlv: Result := FormatDWORD (GetDWORD, 'EAX, ', '');
    ES: Result := 'ES';
    CS: Result := 'CS';
    DS: Result := 'DS';
    SS: Result := 'SS';
    eAX..eDI :
      Result := string (gRegisters [r32, Integer (Optype) - Integer (eAX)]);
    PUSHAD: ;
    POPAD: ;
    GvMa: ;
    EwGw: ;
    FS: Result := 'FS';
    GS: Result := 'GS';
    lv: Result := FormatDWORD (GetDWORD, '', '');
    GvEvlv: ;
    lb: Result := FormatByte (GetByte, '', '');
    GvEvlb: ;
    YbDX: ;
    YvDX: ;
    DXXb: ;
    DXXv: ;
    EwSw: ;
    GvM:
      EmitMODRM (GetOperand, r32, true);
    SwEw: ;
    Ev: EmitMODRM (GetOperand, r32, false);
    Eb: EmitMODRM (GetOperand, r8, false);

    eAXCX: Result := 'EAX, ECX';
    eAXDX: Result := 'EAX, EDX';
    eAXBX: Result := 'EAX, EBX';
    eAXSP: Result := 'EAX, ESP';
    eAXBP: Result := 'EAX, EBP';
    eAXSI: Result := 'EAX, ESI';
    eAXDI: Result := 'EAX, EDI';
    Ap :;
    Fv :;   // arg to pushf/popf - = none

    AlOb: Result := 'al, dWord ptr [' + IntToHex (GetDWORD, 8) + ']';
    eAXOv: Result := 'eax, dWord ptr [' + IntToHex (GetDWORD, 8) + ']';

    ObAL: Result := '[' + IntToHex (GetDWORD, 8) + '], al';
    OvEAX:  Result := '[' + IntToHex (GetDWORD, 8) + '], eax';

    XbYb:; // arg to MOVSB
    XwYw:; // arg to MOVSW
    YbAl,  //        STOSB
    YveAX, //        STOSW
    AlXb,  //        LODSB
    eAXXv, //        LODSW
    AlYb,  //        CMPSB
    eAXYv, //        CMPSW

    mal: Result := FormatByte (GetByte, 'AL, ', '');
    mcl: Result := FormatByte (GetByte, 'CL, ', '');
    mdl: Result := FormatByte (GetByte, 'DL, ', '');
    mbl: Result := FormatByte (GetByte, 'BL, ', '');
    mah: Result := FormatByte (GetByte, 'AH, ', '');
    mch: Result := FormatByte (GetByte, 'CH, ', '');
    mdh: Result := FormatByte (GetByte, 'DH, ', '');
    mbh: Result := FormatByte (GetByte, 'BH, ', '');

    meax: Result := FormatDWORD (GetDWORD, 'EAX, ', '');
    mecx: Result := FormatDWORD (GetDWORD, 'ECX, ', '');
    medx: Result := FormatDWORD (GetDWORD, 'EDX, ', '');
    mebx: Result := FormatDWORD (GetDWORD, 'EBX, ', '');
    mesp: Result := FormatDWORD (GetDWORD, 'ESP, ', '');
    mebp: Result := FormatDWORD (GetDWORD, 'EBP, ', '');
    mesi: Result := FormatDWORD (GetDWORD, 'ESI, ', '');
    medi: Result := FormatDWORD (GetDWORD, 'EDI, ', '');

    lw: Result := FormatWord (GetWord, '', '');
    GvMp :;
    LwLb :;

    jb :
      begin
        i := ShortInt(GetByte);
        Result := IntToHex (Integer (FP) - Integer (FMem) + i, 2);

      end;

    eaxlb :;
    lbal :;
    lbeax :;

    jv :
      begin
        i := Integer (GetDWORD);
        Result := IntToHex (Integer (FP) - Integer (FMem) + i, 2);
      end;
    aldx: Result := 'al, dx';
    dxal: Result := 'dx, al';
    dxeax: Result := 'dx, eax';
    GvEw:;

    RdCd,
    RdDd,
    CdRd,
    DdRd,
    PqQd,

    EvGvLb,
    EvGvCL,

    Ep :;
    Mp :;
    PdEd :;
    PqQq :;
    EdPd :;
    QqPq :;


  end;
end;

destructor TDisassembler.Destroy;
begin
  ReallocMem (FMem, 0);

  inherited;
end;

procedure TDisassembler.DecodeGroup5 (var st, arg: string);
var
  b, b1: byte;
begin
  if fLeft < 1 then
    raise Exception.Create('End of data');

  b := FP^;
  Inc(FP);
  Dec(fLeft);

  b1 := b shr 3 and 7;
  b := b and $c7;

  case b1 of
    $0: begin st := 'inc'; arg := DecodeOp (Ev, true, b); end;
    $1: begin st := 'dec'; arg := DecodeOp (Ev, true, b); end;
    $2: begin st := 'call'; arg := DecodeOp (Ev, true, b); end;
    $3: begin st := 'call'; arg := DecodeOp (Ep, true, b); end;
    $4: begin st := 'jmp'; arg := DecodeOp (Ev, true, b); end;
    $5: begin st := 'jmp'; arg := DecodeOp (Ep, true, b); end;
    $6: begin st := 'push'; arg := DecodeOp (Ev, true, b); end;
  end
end;

procedure TDisassembler.ApplyRelocs;
var
  p: PSectionReloc;
  i (* , va *): Integer;
//  symbol: TSymbol;
begin
  p := FRelocs;

  for i:= 0 to FRelocCount - 1 do
  begin
//    va := p^.virtualAddress;
//    symbol := TSymbol (FSymbols [p^.symbolTableIndex]);

    case p^._type of
      IMAGE_REL_I386_ABSOLUTE :;
      IMAGE_REL_I386_DIR32 :;
      IMAGE_REL_I386_DIR32NB :;
      IMAGE_REL_I386_REL32 :;
      else
        raise Exception.Create ('Unsupported relocation type');
    end;

    Inc(p);
  end
end;

constructor TDisassembler.Create(AMem: PByte; ALen: Integer; ARelocs: PSectionReloc; ARelocCount: Integer; ASymbols: TList);
begin
  ReallocMem (FMem, ALen);
  Move (AMem^, FMem^, ALen);
  FLen := ALen;
  FRelocs := ARelocs;
  FRelocCount := ARelocCount;
  FSymbols := ASymbols;

  ApplyRelocs;
end;

procedure TDisassembler.DecodeGroup1 (Optype: Ti386OpType; var st, arg: string);
var
  b, b1: byte;
begin
  if fLeft < 1 then
    raise Exception.Create('End of data');

  b := FP^;
  Inc(FP);
  Dec(fLeft);

  b1 := b shr 3 and 7;
  b := b and $c7;

  case b1 of
    $0: begin st := 'add'; arg := DecodeOp (Optype, true, b); end;
    $1: begin st := 'or'; arg := DecodeOp (Optype, true, b); end;
    $2: begin st := 'adc'; arg := DecodeOp (Optype, true, b); end;
    $3: begin st := 'sbb'; arg := DecodeOp (Optype, true, b); end;
    $4: begin st := 'and'; arg := DecodeOp (Optype, true, b); end;
    $5: begin st := 'sub'; arg := DecodeOp (Optype, true, b); end;
    $6: begin st := 'xor'; arg := DecodeOp (Optype, true, b); end;
    $7: begin st := 'cmp'; arg := DecodeOp (Optype, true, b); end;
  end
end;

procedure TDisassembler.Disassemble(s: TStream);
var
  Base, nb: Integer;
  op, param: string;
begin
  Base := 0;
  while Base < FLen do
  begin
    nb := Disassemble (Base, op, param);
    if nb = 0 then
      break;

    op := Format('%8.8d'#9'%-8.8s'#9'%s'#13#10, [Base, op, param]);
    s.Write(op [1], Length (op) * sizeof (char));

    Inc(Base, nb);
  end
end;

function TDisassembler.Disassemble(Base: Integer; var op, param: string): Integer;
var
  ost: string;
  optxt: string;
  opcode: Ti386Opcode;
  baseP: PByte;

label
  1;

begin
  try
    FP := FMem;
    Inc(FP, Base);
    baseP := FP;

    fLeft := FLen;
    Dec(fLeft, Base);

    FSeg := srDS;
    FOpSize := os32;

1:  if fLeft > 0 then
    begin
      opcode := gi386OpCodes [FP^];
      case opcode.Optype of
        i2Byte :
          begin
            Inc(FP);
            Dec(fLeft);
            opcode := g2Bytei386OpCodes [FP^]
          end;
        iES,
        iCS,
        iSS,
        iDS,
        iFS,
        iGS :
          begin
            case opcode.Optype of
              iES: FSeg := srES;
              iCS: FSeg := srCS;
              iSS: FSeg := srSS;
              iDS: FSeg := srDS;
              iFS: FSeg := srFS;
              iGS: FSeg := srGS;
            end;
            Inc(FP);
            Dec(fLeft);
            goto 1;
          end;

        oSize,
        aSize :
          begin
            case opcode.Optype of
              oSize: FOpSize := os16
            end;
            Inc(FP);
            Dec(fLeft);
            goto 1;
          end;

      end;

      Inc(FP);
      Dec(fLeft);

      with opcode do
      begin

        if txt [1] = '~' then
        case txt [2] of
          '1': DecodeGroup1 (Optype, ost, optxt);
          '4': ;
          '5': DecodeGroup5 (ost, optxt)
        end
        else
        begin
          ost := string (txt);
          optxt := DecodeOp (Optype, false, 0);
        end
      end;

      op := ost;
      param := optxt;
      Result := Integer (FP) - Integer (baseP);
    end
    else
      Result := 0;
  except
    Result := 0
  end;
end;

procedure TDisassembler.SizeGroup1(Optype: Ti386OpType);
var
  b: byte;
begin
  if fLeft < 1 then
    raise Exception.Create('End of data');

  b := FP^;
  Inc(FP);
  Dec(fLeft);

  b := b and $c7;

  DecodeOp (Optype, true, b)
end;

procedure TDisassembler.SizeGroup5;
begin
  SizeGroup1 (Ev)
end;

function TDisassembler.SizeOfInstruction (Base: Integer): Integer;
var
  opcode: Ti386Opcode;
  baseP: PByte;

label
    1;

begin
  try
    FP := FMem;
    Inc(FP, Base);
    baseP := FP;

    fLeft := FLen;
    Dec(fLeft, Base);

    FSeg := srDS;
    FOpSize := os32;

1:  if fLeft > 0 then
    begin
      opcode := gi386OpCodes [FP^];

      case opcode.Optype of
        i2Byte :
          begin
            Inc(FP);
            Dec(fLeft);
            opcode := g2Bytei386OpCodes [FP^]
          end;

        iES,
        iCS,
        iSS,
        iDS,
        iFS,
        iGS :
          begin
            case opcode.Optype of
              iES: FSeg := srES;
              iCS: FSeg := srCS;
              iSS: FSeg := srSS;
              iDS: FSeg := srDS;
              iFS: FSeg := srFS;
              iGS: FSeg := srGS;
            end;
            Inc(FP);
            Dec(fLeft);
            goto 1;
          end;

        oSize,
        aSize :
          begin
            case opcode.Optype of
              oSize: FOpSize := os16
            end;
            Inc(FP);
            Dec(fLeft);
            goto 1;
          end;


      end;

      Inc(FP);
      Dec(fLeft);

      with opcode do
      begin

        if txt [1] = '~' then
        case txt [2] of
          '1': SizeGroup1 (Optype);
          '4': ;
          '5': SizeGroup5
        end
        else
          DecodeOp (Optype, false, 0);
      end;

      Result := Integer (FP) - Integer (baseP);
    end
    else
      Result := 0;
  except
    Result := 0
  end;
end;

procedure TDisassembler.GetLineMap(lineMap: TList);
var
  Base, nb: Integer;
  p: PByte;
begin
  lineMap.Clear;
  Base := 0;
  while Base < FLen do
  begin
    nb := SizeOfInstruction (Base);
    if nb = 0 then
      break;

    p := FMem;
    Inc(p, Base);
    lineMap.Add(p);
    Inc(Base, nb)
  end
end;

end.
