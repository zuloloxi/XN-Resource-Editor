unit unitDisassembler;

interface

uses Windows, Classes, SysUtils, unitPEFile;

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
  fLen, fLeft : Integer;
  fP, fMem : PByte;
  fSeg : TSegReg;
  fOpSize : TOpSize;

  fRelocs : PSectionReloc;
  fRelocCount : Integer;

  fSymbols : TList;

  function DecodeOp(optype: Ti386OpType; alreadyHasOperand: boolean;
      operand: byte): string;
  procedure DecodeGroup1(optype: Ti386OpType; var st, arg: string);
  procedure DecodeGroup5(var st, arg: string);

  function SizeOfInstruction (base : Integer) : Integer;
  procedure SizeGroup1 (optype : Ti386OpType);
  procedure SizeGroup5;

  procedure ApplyRelocs;

public
  constructor Create (AMem : PByte; ALen : Integer; ARelocs : PSectionReloc; ARelocCount : Integer; ASymbols : TList);
  destructor Destroy; override;

  function Disassemble(base : Integer; var op, param : string) : Integer; overload;
  procedure Disassemble (s : TStream); overload;
  procedure GetLineMap (lineMap : TList);

  property Memory : PByte read fMem;
  property MemorySize : Integer read fLen;
end;


implementation

type

Ti386OpCode = packed record
  op : byte;
  txt : string [13];
  optype : Ti386OpType;
end;

var
  gi386OpCodes : array [0..255] of Ti386OpCode = (
    (op:$00; txt:'add';    optype:EbGb),
    (op:$01; txt:'add';    optype:EvGv),
    (op:$02; txt:'add';    optype:GbEb),
    (op:$03; txt:'add';    optype:GvEv),
    (op:$04; txt:'add';    optype:AlLb),
    (op:$05; txt:'add';    optype:eAXLv),
    (op:$06; txt:'push';   optype:ES),
    (op:$07; txt:'pop';    optype:ES),

    (op:$08; txt:'or';     optype:EbGb),
    (op:$09; txt:'or';     optype:EvGv),
    (op:$0A; txt:'or';     optype:GbEb),
    (op:$0B; txt:'or';     optype:GvEv),
    (op:$0C; txt:'or';     optype:AlLb),
    (op:$0D; txt:'or';     optype:eAXLv),
    (op:$0E; txt:'push';   optype:CS),
    (op:$0F; txt:'';       optype:i2Byte),  // 2 byte opcode indicator

    (op:$10; txt:'ADC';    optype:EbGb),
    (op:$11; txt:'ADC';    optype:EvGv),
    (op:$12; txt:'ADC';    optype:GbEb),
    (op:$13; txt:'ADC';    optype:GvEv),
    (op:$14; txt:'ADC';    optype:AlLb),
    (op:$15; txt:'ADC';    optype:eAXLv),
    (op:$16; txt:'PUSH';   optype:SS),
    (op:$17; txt:'POP';    optype:SS),

    (op:$18; txt:'SBB';    optype:EbGb),
    (op:$19; txt:'SBB';    optype:EvGv),
    (op:$1A; txt:'SBB';    optype:GbEb),
    (op:$1B; txt:'SBB';    optype:GvEv),
    (op:$1C; txt:'SBB';    optype:AlLb),
    (op:$1D; txt:'SBB';    optype:eAXLv),
    (op:$1E; txt:'PUSH';   optype:DS),
    (op:$1F; txt:'POP';    optype:DS),

    (op:$20; txt:'AND';    optype:EbGb),
    (op:$21; txt:'AND';    optype:EvGv),
    (op:$22; txt:'AND';    optype:GbEb),
    (op:$23; txt:'AND';    optype:GvEv),
    (op:$24; txt:'AND';    optype:AlLb),
    (op:$25; txt:'AND';    optype:eAXLv),
    (op:$26; txt:'SEG';    optype:iES),
    (op:$27; txt:'DAA';    optype:None),

    (op:$28; txt:'SUB';    optype:EbGb),
    (op:$29; txt:'SUB';    optype:EvGv),
    (op:$2A; txt:'SUB';    optype:GbEb),
    (op:$2B; txt:'SUB';    optype:GvEv),
    (op:$2C; txt:'SUB';    optype:AlLb),
    (op:$2D; txt:'SUB';    optype:eAXLv),
    (op:$2E; txt:'SEG';    optype:iCS),
    (op:$2F; txt:'DAS';    optype:None),

    (op:$30; txt:'xor';    optype:EbGb),
    (op:$31; txt:'xor';    optype:EvGv),
    (op:$32; txt:'xor';    optype:GbEb),
    (op:$33; txt:'xor';    optype:GvEv),
    (op:$34; txt:'xor';    optype:AlLb),
    (op:$35; txt:'xor';    optype:eAXLv),
    (op:$36; txt:'seg';    optype:iSS),
    (op:$37; txt:'aaa';    optype:None),

    (op:$38; txt:'cmp';    optype:EbGb),
    (op:$39; txt:'cmp';    optype:EvGv),
    (op:$3A; txt:'cmp';    optype:GbEb),
    (op:$3B; txt:'cmp';    optype:GvEv),
    (op:$3C; txt:'cmp';    optype:AlLb),
    (op:$3D; txt:'cmp';    optype:eAXLv),
    (op:$3E; txt:'seg';    optype:iDS),
    (op:$3F; txt:'aas';    optype:None),

    (op:$40; txt:'INC';    optype:eAX),
    (op:$41; txt:'INC';    optype:eCX),
    (op:$42; txt:'INC';    optype:eDX),
    (op:$43; txt:'INC';    optype:eBX),
    (op:$44; txt:'INC';    optype:eSP),
    (op:$45; txt:'INC';    optype:eBP),
    (op:$46; txt:'INC';    optype:eSI),
    (op:$47; txt:'INC';    optype:eDI),

    (op:$48; txt:'DEC';    optype:eAX),
    (op:$49; txt:'DEC';    optype:eCX),
    (op:$4A; txt:'DEC';    optype:eDX),
    (op:$4B; txt:'DEC';    optype:eBX),
    (op:$4C; txt:'DEC';    optype:eSP),
    (op:$4D; txt:'DEC';    optype:eBP),
    (op:$4E; txt:'DEC';    optype:eSI),
    (op:$4F; txt:'DEC';    optype:eDI),

    (op:$50; txt:'push';    optype:eAX),
    (op:$51; txt:'push';    optype:eCX),
    (op:$52; txt:'push';    optype:eDX),
    (op:$53; txt:'push';    optype:eBX),
    (op:$54; txt:'push';    optype:eSP),
    (op:$55; txt:'push';    optype:eBP),
    (op:$56; txt:'push';    optype:eSI),
    (op:$57; txt:'push';    optype:eDI),

    (op:$58; txt:'pop';    optype:eAX),
    (op:$59; txt:'pop';    optype:eCX),
    (op:$5A; txt:'pop';    optype:eDX),
    (op:$5B; txt:'pop';    optype:eBX),
    (op:$5C; txt:'pop';    optype:eSP),
    (op:$5D; txt:'pop';    optype:eBP),
    (op:$5E; txt:'pop';    optype:eSI),
    (op:$5F; txt:'pop';    optype:eDI),

    (op:$60; txt:'PUSHA';  optype:PUSHAD),
    (op:$61; txt:'POPA';   optype:POPAD),
    (op:$62; txt:'BOUND';  optype:GvMa),
    (op:$63; txt:'ARPL';   optype:EwGw),
    (op:$64; txt:'SEG';    optype:iFS),
    (op:$65; txt:'SEG';    optype:iGS),
    (op:$66; txt:'Operand';optype:OSize),
    (op:$67; txt:'Operand';optype:ASize),

    (op:$68; txt:'push';   optype:lv),
    (op:$69; txt:'imul';   optype:GvEvlv),
    (op:$6A; txt:'push';   optype:lb),
    (op:$6B; txt:'imul';   optype:GvEvlb),
    (op:$6C; txt:'insb';   optype:YbDX),
    (op:$6D; txt:'insd';  optype:YvDX),
    (op:$6E; txt:'outsb';  optype:DxXb),
    (op:$6F; txt:'outsd'; optype:DxXv),

    (op:$70; txt:'jo'; optype:jb),
    (op:$71; txt:'jno'; optype:jb),
    (op:$72; txt:'jb'; optype:jb),
    (op:$73; txt:'jnb'; optype:jb),
    (op:$74; txt:'je'; optype:jb),
    (op:$75; txt:'jne'; optype:jb),
    (op:$76; txt:'jbe'; optype:jb),
    (op:$77; txt:'jnbe'; optype:jb),

    (op:$78; txt:'js'; optype:jb),
    (op:$79; txt:'jns'; optype:jb),
    (op:$7A; txt:'jp'; optype:jb),
    (op:$7B; txt:'jnp'; optype:jb),
    (op:$7C; txt:'jl'; optype:jb),
    (op:$7D; txt:'jnl'; optype:jb),
    (op:$7E; txt:'jle'; optype:jb),
    (op:$7F; txt:'jnle'; optype:jb),

    (op:$80; txt:'~1'; optype:eblb),
    (op:$81; txt:'~1'; optype:evlv),
    (op:$82; txt:'~1'; optype:evlb),
    (op:$83; txt:'~1'; optype:eblv),
    (op:$84; txt:'TEST'; optype:Ebgb),
    (op:$85; txt:'TEST'; optype:Evgv),
    (op:$86; txt:'XCHG'; optype:Ebgb),
    (op:$87; txt:'XCHG'; optype:Evgv),

    (op:$88; txt:'MOV'; optype:Ebgb),
    (op:$89; txt:'MOV'; optype:Evgv),
    (op:$8A; txt:'MOV'; optype:GbEb),
    (op:$8B; txt:'MOV'; optype:GvEv),
    (op:$8C; txt:'MOV'; optype:EwSw),
    (op:$8D; txt:'LEA'; optype:GvM),
    (op:$8E; txt:'MOV'; optype:SwEw),
    (op:$8F; txt:'POP'; optype:Ev),

    (op:$90; txt:'NOP'; optype:None),
    (op:$91; txt:'XCHG'; optype:eAXCX),
    (op:$92; txt:'XCHG'; optype:eAXDX),
    (op:$93; txt:'XCHG'; optype:eAXBX),
    (op:$94; txt:'XCHG'; optype:eAXSP),
    (op:$95; txt:'XCHG'; optype:eAXBP),
    (op:$96; txt:'XCHG'; optype:eAXSI),
    (op:$97; txt:'XCHG'; optype:eAXDI),

    (op:$98; txt:'CBW'; optype:None),
    (op:$99; txt:'CWD'; optype:None),
    (op:$9A; txt:'CALL'; optype:Ap),
    (op:$9B; txt:'WAIT'; optype:None),
    (op:$9C; txt:'PUSHF'; optype:Fv),
    (op:$9D; txt:'POPF'; optype:Fv),
    (op:$9E; txt:'SAHF'; optype:None),
    (op:$9F; txt:'LAHF'; optype:None),

    (op:$A0; txt:'MOV'; optype:ALOb),
    (op:$A1; txt:'MOV'; optype:eAXOv),
    (op:$A2; txt:'MOV'; optype:ObAL),
    (op:$A3; txt:'MOV'; optype:OvEAX),
    (op:$A4; txt:'MOVSB'; optype:XbYb),
    (op:$A5; txt:'MOVSW'; optype:XwYw),
    (op:$A6; txt:'CMPSB'; optype:XbYb),
    (op:$A7; txt:'CMPSW'; optype:XwYw),

    (op:$A8; txt:'TEST'; optype:Allb),
    (op:$A9; txt:'TEST'; optype:eAXLv),
    (op:$AA; txt:'STOSB'; optype:YbAl),
    (op:$AB; txt:'STOSW'; optype:YveAX),
    (op:$AC; txt:'LODSB'; optype:AlXb),
    (op:$AD; txt:'LODWSW'; optype:eAXXv),
    (op:$AE; txt:'SCASB'; optype:AlYb),
    (op:$AF; txt:'SCASW'; optype:eAXYv),

    (op:$B0; txt:'MOV'; optype:mAl),
    (op:$B1; txt:'MOV'; optype:mCl),
    (op:$B2; txt:'MOV'; optype:mDl),
    (op:$B3; txt:'MOV'; optype:mBl),
    (op:$B4; txt:'MOV'; optype:mAh),
    (op:$B5; txt:'MOV'; optype:mCh),
    (op:$B6; txt:'MOV'; optype:mDh),
    (op:$B7; txt:'MOV'; optype:mBh),

    (op:$B8; txt:'MOV'; optype:meax),
    (op:$B9; txt:'MOV'; optype:mecx),
    (op:$BA; txt:'MOV'; optype:medx),
    (op:$BB; txt:'MOV'; optype:mebx),
    (op:$BC; txt:'MOV'; optype:mesp),
    (op:$BD; txt:'MOV'; optype:mebp),
    (op:$BE; txt:'MOV'; optype:mesi),
    (op:$BF; txt:'MOV'; optype:medi),

    (op:$C0; txt:'~2'; optype:eblb),
    (op:$C1; txt:'~2'; optype:evlb),
    (op:$C2; txt:'RET'; optype:lw),   { near ret }
    (op:$C3; txt:'RET'; optype:None),
    (op:$C4; txt:'LES'; optype:GvMp),
    (op:$C5; txt:'LDS'; optype:GvMp),
    (op:$C6; txt:'MOV'; optype:Eblb),
    (op:$C7; txt:'MOV'; optype:EvLv),

    (op:$C8; txt:'ENTER'; optype:LwLb),
    (op:$C9; txt:'LEAVE'; optype:None),
    (op:$CA; txt:'RET'; optype:lw),  { far ret }
    (op:$CB; txt:'RET'; optype:None),
    (op:$CC; txt:'INT 3'; optype:None),
    (op:$CD; txt:'INT'; optype:lb),
    (op:$CE; txt:'INTO'; optype:None),
    (op:$CF; txt:'IRET'; optype:None),

    (op:$D0; txt:'~2'; optype:eb1),
    (op:$D1; txt:'~2'; optype:ev1),
    (op:$D2; txt:'~2'; optype:ebcl),
    (op:$D3; txt:'~2'; optype:evcl),
    (op:$D4; txt:'AAM'; optype:None),
    (op:$D5; txt:'AAD'; optype:None),
    (op:$D6; txt:''; optype:None),
    (op:$D7; txt:'XLAT'; optype:None),

    (op:$D8; txt:''; optype:Escape),
    (op:$D9; txt:''; optype:Escape),
    (op:$DA; txt:''; optype:Escape),
    (op:$DB; txt:''; optype:Escape),
    (op:$DC; txt:''; optype:Escape),
    (op:$DD; txt:''; optype:Escape),
    (op:$DE; txt:''; optype:Escape),
    (op:$DF; txt:''; optype:Escape),

    (op:$E0; txt:'LOOPN'; optype:Jb),
    (op:$E1; txt:'LOOPE'; optype:Jb),
    (op:$E2; txt:'LOOP'; optype:Jb),
    (op:$E3; txt:'JCXZ'; optype:Jb),
    (op:$E4; txt:'IN'; optype:Allb),
    (op:$E5; txt:'IN'; optype:eaxlb),
    (op:$E6; txt:'OUT'; optype:lbal),
    (op:$E7; txt:'LOOPN'; optype:lbeax),

    (op:$E8; txt:'CALL'; optype:Jv),
    (op:$E9; txt:'JMP'; optype:Jv),
    (op:$EA; txt:'JMP'; optype:Ap),
    (op:$EB; txt:'JMP'; optype:Jb),
    (op:$EC; txt:'IN'; optype:aldx),
    (op:$ED; txt:'IN'; optype:eaxdx),
    (op:$EE; txt:'OUT'; optype:dxal),
    (op:$EF; txt:'OUT'; optype:dxeax),

    (op:$F0; txt:'LOCK'; optype:none), { prefix }
    (op:$F1; txt:''; optype:none),
    (op:$F2; txt:'REPNE'; optype:none), { prefix }
    (op:$F3; txt:'REP'; optype:none), { prefix }
    (op:$F4; txt:'HLT'; optype:none),
    (op:$F5; txt:'CMC'; optype:none),
    (op:$F6; txt:'~3'; optype:eb),
    (op:$F7; txt:'~3'; optype:ev),

    (op:$F8; txt:'CLC'; optype:none),
    (op:$F9; txt:'STC'; optype:none),
    (op:$FA; txt:'CLI'; optype:none),
    (op:$FB; txt:'STI'; optype:none),
    (op:$FC; txt:'CLD'; optype:none),
    (op:$FD; txt:'STD'; optype:none),
    (op:$FE; txt:'~4'; optype:none),
    (op:$FF; txt:'~5'; optype:none)
  );

  g2Bytei386OpCodes : array [0..255] of Ti386OpCode = (
    (op:$00; txt:'~6'; optype:none),
    (op:$01; txt:''; optype:none),
    (op:$02; txt:'LAR'; optype:GvEw),
    (op:$03; txt:'LSL'; optype:GvEw),
    (op:$04; txt:''; optype:none),
    (op:$05; txt:''; optype:none),
    (op:$06; txt:'CLTS'; optype:none),
    (op:$07; txt:''; optype:none),

    (op:$08; txt:'INVD'; optype:none),
    (op:$09; txt:'WBINVD'; optype:none),
    (op:$0a; txt:''; optype:none),
    (op:$0b; txt:'UD2'; optype:none),
    (op:$0c; txt:''; optype:none),
    (op:$0d; txt:''; optype:none),
    (op:$0e; txt:''; optype:none),
    (op:$0f; txt:''; optype:none),

    (op:$10; txt:''; optype:none),
    (op:$11; txt:''; optype:none),
    (op:$12; txt:''; optype:none),
    (op:$13; txt:''; optype:none),
    (op:$14; txt:''; optype:none),
    (op:$15; txt:''; optype:none),
    (op:$16; txt:''; optype:none),
    (op:$17; txt:''; optype:none),

    (op:$18; txt:''; optype:none),
    (op:$19; txt:''; optype:none),
    (op:$1a; txt:''; optype:none),
    (op:$1b; txt:''; optype:none),
    (op:$1c; txt:''; optype:none),
    (op:$1d; txt:''; optype:none),
    (op:$1e; txt:''; optype:none),
    (op:$1f; txt:''; optype:none),

    (op:$20; txt:'MOV'; optype:RdCd),
    (op:$21; txt:'MOV'; optype:RdDd),
    (op:$22; txt:'MOV'; optype:CdRd),
    (op:$23; txt:'MOV'; optype:DdRd),
    (op:$24; txt:''; optype:none),
    (op:$25; txt:''; optype:none),
    (op:$26; txt:''; optype:none),
    (op:$27; txt:''; optype:none),

    (op:$28; txt:''; optype:none),
    (op:$29; txt:''; optype:none),
    (op:$2a; txt:''; optype:none),
    (op:$2b; txt:''; optype:none),
    (op:$2c; txt:''; optype:none),
    (op:$2d; txt:''; optype:none),
    (op:$2e; txt:''; optype:none),
    (op:$2f; txt:''; optype:none),

    (op:$30; txt:'WRMSR'; optype:none),
    (op:$31; txt:'RDTSC'; optype:none),
    (op:$32; txt:'RDMSR'; optype:none),
    (op:$33; txt:'RDPMC'; optype:none),
    (op:$34; txt:''; optype:none),
    (op:$35; txt:''; optype:none),
    (op:$36; txt:''; optype:none),
    (op:$37; txt:''; optype:none),

    (op:$38; txt:''; optype:none),
    (op:$39; txt:''; optype:none),
    (op:$3a; txt:''; optype:none),
    (op:$3b; txt:''; optype:none),
    (op:$3c; txt:''; optype:none),
    (op:$3d; txt:''; optype:none),
    (op:$3e; txt:''; optype:none),
    (op:$3f; txt:''; optype:none),

    (op:$40; txt:'CMOVO'; optype:GvEv),
    (op:$41; txt:'CMOVNO'; optype:GvEv),
    (op:$42; txt:'CMOVB'; optype:GvEv),
    (op:$43; txt:'CMOVAE'; optype:GvEv),
    (op:$44; txt:'CMOVE'; optype:GvEv),
    (op:$45; txt:'CMOVNE'; optype:GvEv),
    (op:$46; txt:'CMOVBE'; optype:GvEv),
    (op:$47; txt:'CMOVA'; optype:GvEv),

    (op:$48; txt:'CMOVS'; optype:GvEv),
    (op:$49; txt:'CMOVNS'; optype:GvEv),
    (op:$4a; txt:'CMOVP'; optype:GvEv),
    (op:$4b; txt:'CMOVNP'; optype:GvEv),
    (op:$4c; txt:'CMOVL'; optype:GvEv),
    (op:$4d; txt:'CMOVGE'; optype:GvEv),
    (op:$4e; txt:'CMOVLE'; optype:GvEv),
    (op:$4f; txt:'CMOVG'; optype:GvEv),

    (op:$50; txt:''; optype:none),
    (op:$51; txt:''; optype:none),
    (op:$52; txt:''; optype:none),
    (op:$53; txt:''; optype:none),
    (op:$54; txt:''; optype:none),
    (op:$55; txt:''; optype:none),
    (op:$56; txt:''; optype:none),
    (op:$57; txt:''; optype:none),

    (op:$58; txt:''; optype:none),
    (op:$59; txt:''; optype:none),
    (op:$5a; txt:''; optype:none),
    (op:$5b; txt:''; optype:none),
    (op:$5c; txt:''; optype:none),
    (op:$5d; txt:''; optype:none),
    (op:$5e; txt:''; optype:none),
    (op:$5f; txt:''; optype:none),

    (op:$60; txt:'PUNPCKLWB'; optype:PqQd),
    (op:$61; txt:'PUNPCKLWD'; optype:PqQd),
    (op:$62; txt:'PUNOCKLDQ'; optype:PqQd),
    (op:$63; txt:'PACKUSDW'; optype:PqQd),
    (op:$64; txt:'PCMPGTB'; optype:PqQd),
    (op:$65; txt:'PCMPGTW'; optype:PqQd),
    (op:$66; txt:'PCMPGTD'; optype:PqQd),
    (op:$67; txt:'PACKSSWB'; optype:PqQd),

    (op:$68; txt:'PUNPCKHWB'; optype:PqQd),
    (op:$69; txt:'PUNPCKHWD'; optype:PqQd),
    (op:$6a; txt:'PUNPCKHWQ'; optype:PqQd),
    (op:$6b; txt:'PACKSSDW'; optype:PqQd),
    (op:$6c; txt:''; optype:none),
    (op:$6d; txt:''; optype:none),
    (op:$6e; txt:'MOVD'; optype:PdEd),
    (op:$6f; txt:'MOVQ'; optype:PqQq),

    (op:$70; txt:''; optype:none),
    (op:$71; txt:'PSHIMW'; optype:none),
    (op:$72; txt:'PSHIMD'; optype:none),
    (op:$73; txt:'PSHIMQ'; optype:none),
    (op:$74; txt:'PCMPEQB'; optype:PqQd),
    (op:$75; txt:'PCMPEQW'; optype:PqQd),
    (op:$76; txt:'PCMPEQD'; optype:PqQd),
    (op:$77; txt:'EMMS'; optype:none),

    (op:$78; txt:''; optype:none),
    (op:$79; txt:''; optype:none),
    (op:$7a; txt:''; optype:none),
    (op:$7b; txt:''; optype:none),
    (op:$7c; txt:''; optype:none),
    (op:$7d; txt:''; optype:none),
    (op:$7e; txt:'MOVD'; optype:EdPd),
    (op:$7f; txt:'MOVQ'; optype:QqPq),

    (op:$80; txt:'jo'; optype:jv),
    (op:$81; txt:'jno'; optype:jv),
    (op:$82; txt:'jb'; optype:jv),
    (op:$83; txt:'jnb'; optype:jv),
    (op:$84; txt:'je'; optype:jv),
    (op:$85; txt:'jne'; optype:jv),
    (op:$86; txt:'jbe'; optype:jv),
    (op:$87; txt:'jnbe'; optype:jv),

    (op:$88; txt:'js'; optype:jv),
    (op:$89; txt:'jns'; optype:jv),
    (op:$8A; txt:'jp'; optype:jv),
    (op:$8B; txt:'jnp'; optype:jv),
    (op:$8C; txt:'jl'; optype:jv),
    (op:$8D; txt:'jnl'; optype:jv),
    (op:$8E; txt:'jle'; optype:jv),
    (op:$8F; txt:'jnle'; optype:jv),

    (op:$90; txt:'seto'; optype:Eb),
    (op:$91; txt:'setno'; optype:Eb),
    (op:$92; txt:'setb'; optype:Eb),
    (op:$93; txt:'setnb'; optype:Eb),
    (op:$94; txt:'sete'; optype:Eb),
    (op:$95; txt:'setne'; optype:Eb),
    (op:$96; txt:'setbe'; optype:Eb),
    (op:$97; txt:'setnbe'; optype:Eb),

    (op:$98; txt:'SETS'; optype:Eb),
    (op:$99; txt:'SETNS'; optype:Eb),
    (op:$9a; txt:'SETP'; optype:Eb),
    (op:$9b; txt:'SETNP'; optype:Eb),
    (op:$9c; txt:'SETL'; optype:Eb),
    (op:$9d; txt:'SETNL'; optype:Eb),
    (op:$9e; txt:'SETLE'; optype:Eb),
    (op:$9f; txt:'SETNLE'; optype:Eb),

    (op:$a0; txt:'PUSH'; optype:FS),
    (op:$a1; txt:'POP'; optype:FS),
    (op:$a2; txt:'CPUID'; optype:none),
    (op:$a3; txt:'BT'; optype:EvGv),
    (op:$a4; txt:'SHLD'; optype:EvGvLb),
    (op:$a5; txt:'SHLD'; optype:EvGvCL),
    (op:$a6; txt:''; optype:none),
    (op:$a7; txt:''; optype:none),

    (op:$a8; txt:'PUSH'; optype:GS),
    (op:$a9; txt:'POP'; optype:GS),
    (op:$aa; txt:'RSM'; optype:none),
    (op:$ab; txt:'BTS'; optype:EvGv),
    (op:$ac; txt:'SHRD'; optype:EvGvLb),
    (op:$ad; txt:'SHRD'; optype:EvGvCL),
    (op:$ae; txt:''; optype:none),
    (op:$af; txt:'IMUL'; optype:GvEv),

    (op:$b0; txt:'CMPXCHG'; optype:EbGb),
    (op:$b1; txt:'CMPXCHG'; optype:EvGv),
    (op:$b2; txt:'LSS'; optype:Mp),
    (op:$b3; txt:'BTR'; optype:EvGv),
    (op:$b4; txt:'LFS'; optype:none),
    (op:$b5; txt:'LGS'; optype:none),
    (op:$b6; txt:'MOVZX'; optype:GbEb),
    (op:$b7; txt:'MOVZX'; optype:GvEw),

    (op:$b8; txt:''; optype:none),
    (op:$b9; txt:''; optype:none),
    (op:$ba; txt:'~8'; optype:none),
    (op:$bb; txt:'BTC'; optype:EvGv),
    (op:$bc; txt:'BSF'; optype:EvGv),
    (op:$bd; txt:'BSR'; optype:EvGv),
    (op:$be; txt:'MOVSX'; optype:GvEb),
    (op:$bf; txt:'MOVSX'; optype:GvEw),

    (op:$c0; txt:'XADD'; optype:EbGb),
    (op:$c1; txt:'XADD'; optype:EvGv),
    (op:$c2; txt:''; optype:none),
    (op:$c3; txt:''; optype:none),
    (op:$c4; txt:''; optype:none),
    (op:$c5; txt:''; optype:none),
    (op:$c6; txt:''; optype:none),
    (op:$c7; txt:'~9'; optype:none),

    (op:$c8; txt:'BSWAP'; optype:EAX),
    (op:$c9; txt:'BSWAP'; optype:ECX),
    (op:$ca; txt:'BSWAP'; optype:EDX),
    (op:$cb; txt:'BSWAP'; optype:EBX),
    (op:$cc; txt:'BSWAP'; optype:ESP),
    (op:$cd; txt:'BSWAP'; optype:EBP),
    (op:$ce; txt:'BSWAP'; optype:ESI),
    (op:$cf; txt:'BSWAP'; optype:EDI),

    (op:$d0; txt:''; optype:none),
    (op:$d1; txt:'PSRLW'; optype:PqQd),
    (op:$d2; txt:'PSRLD'; optype:PqQd),
    (op:$d3; txt:'PSRLQ'; optype:PqQd),
    (op:$d4; txt:''; optype:none),
    (op:$d5; txt:'PMULLW'; optype:PqQd),
    (op:$d6; txt:''; optype:none),
    (op:$d7; txt:''; optype:none),

    (op:$d8; txt:''; optype:none),
    (op:$d9; txt:''; optype:none),
    (op:$da; txt:''; optype:none),
    (op:$db; txt:''; optype:none),
    (op:$dc; txt:''; optype:none),
    (op:$dd; txt:''; optype:none),
    (op:$de; txt:''; optype:none),
    (op:$df; txt:''; optype:none),

    (op:$e0; txt:''; optype:none),
    (op:$e1; txt:'PSRAW'; optype:PqQd),
    (op:$e2; txt:'PSRAD'; optype:PqQd),
    (op:$e3; txt:''; optype:none),
    (op:$e4; txt:''; optype:none),
    (op:$e5; txt:'PMULAW'; optype:PqQd),
    (op:$e6; txt:''; optype:none),
    (op:$e7; txt:''; optype:none),

    (op:$e8; txt:'PSUBUSB'; optype:PqQq),
    (op:$e9; txt:'PSUBUSW'; optype:PqQq),
    (op:$ea; txt:''; optype:none),
    (op:$eb; txt:'PAND'; optype:PqQq),
    (op:$ec; txt:'PADDUSB'; optype:PqQq),
    (op:$ed; txt:'PADDUSW'; optype:PqQq),
    (op:$ee; txt:''; optype:none),
    (op:$ef; txt:'PANDN'; optype:PqQq),

    (op:$f0; txt:''; optype:none),
    (op:$f1; txt:'PSLLW'; optype:PqQd),
    (op:$f2; txt:'PSLLD'; optype:PqQd),
    (op:$f3; txt:'PSLLQ'; optype:PqQd),
    (op:$f4; txt:''; optype:none),
    (op:$f5; txt:'PMADDWD'; optype:PqQd),
    (op:$f6; txt:''; optype:none),
    (op:$f7; txt:''; optype:none),

    (op:$f8; txt:'PSUBSB'; optype:PqQq),
    (op:$f9; txt:'PSUBSW'; optype:PqQq),
    (op:$fa; txt:''; optype:none),
    (op:$fb; txt:'POR'; optype:PqQq),
    (op:$fc; txt:'PADDSB'; optype:PqQq),
    (op:$fd; txt:'PADDSW'; optype:PqQq),
    (op:$fe; txt:''; optype:none),
    (op:$ff; txt:'PXOR'; optype:PqQq)

  );
type
  TRegType = (r8, r16, r32, mm);

var
  gRegisters : array [TRegType, 0..7] of string [3] = (
    ('al', 'cl', 'dl', 'bl', 'ah', 'ch', 'dh', 'bh'),
    ('ax', 'cx', 'dx', 'bx', 'sp', 'bp', 'si', 'di'),
    ('eax', 'ecx', 'edx', 'ebx', 'esp', 'ebp', 'esi', 'edi'),
    ('mm0', 'mm1', 'mm2', 'mm3', 'mm4', 'mm5', 'mm6', 'mm7'));


function TDisassembler.DecodeOp (optype : Ti386OpType; alreadyHasOperand : boolean; operand : byte) : string;
var
  i : Integer;

  procedure CheckLen (requiredLen : Integer);
  begin
    if fLeft < requiredLen then
      raise Exception.Create ('End of data');

  end;

  function GetByte : byte;
  begin
    checkLen (sizeof (byte));
    result := fP^;
    Inc (fP, sizeof (byte));
    Dec (fLeft, sizeof (byte))
  end;

  function GetWord : word;
  begin
    checkLen (sizeof (word));
    result := PWord (fP)^;
    Inc (fP, sizeof (word));
    Dec (fLeft, sizeof (word))
  end;

  function GetDWord : DWord;
  begin
    checkLen (sizeof (DWord));
    result := PDWord (fP)^;
    Inc (fP, sizeof (DWord));
    Dec (fLeft, sizeof (DWord))
  end;

  function GetWordOrDWORD : DWord;
  begin
    if fOpSize = os16 then
      result := GetWord
    else
      result := GetDWORD
  end;


  function FormatByte (b : byte; const prefix, suffix : string) : string;
  begin
    result := prefix  + IntToHex (b, 2 * sizeof (byte)) + suffix;
  end;

  function FormatDWORD (d : DWORD; const prefix, suffix : string) : string;
  begin
    result := prefix  + IntToHex (d, 2 * sizeof (DWORD)) + suffix;
  end;

  function FormatWord (w : word; const prefix, suffix : string) : string;
  begin
    result := prefix  + IntToHex (w, 2 * sizeof (WORD)) + suffix;
  end;

  // method :: 0 = r8, 1 = r16, 2 = r32, 3 = mm
  function DecodeSIB (sib : byte; regType : TRegType) : string;
  var
    ss, idx, md : byte;
  begin
    ss := sib shr 6;
    idx := (sib shr 3) and 7;
    md := sib and 7;


    if md <> 4 then
    begin
      result := string (gRegisters  [regType, md]);
      result := result + '+' + string (gRegisters [r32, idx]);
      case ss of
        1 : result := result + '*2';
        2 : result := result + '*4';
        3 : result := result + '*8';
      end
    end
    else
      result := string (gRegisters  [regType, idx]);
  end;

  function ptrType (reg : TRegType) : string;
  begin
    if fOpSize = os16 then
      reg := r16;

    result := '';
    case reg of
      r8 : result := 'byte ptr ';
      r16 : result := 'word ptr ';
      r32 : result := 'dword ptr '
    end
  end;

  procedure EmitMODRM (operand : byte; rt : TRegType; hasReg : boolean);
  var
    _mod : byte;
    _rm : byte;
    sib : byte;
    reg : byte;
    dw : DWORD;

    function SegMod : string;
    begin
      result := '';
      case fSeg of
        srDS:;
        srES: result := 'es:';
        srCS: result := 'cs:';
        srSS: result := 'ss:';
        srFS: result := 'fs:';
        srGS: result := 'gs:';
      end;
    end;

  begin
   _rm := operand and 7;
   _mod := operand shr 6;
   reg := operand shr 3 and 7;
   result := '';

   if hasReg then
     result := string (gRegisters [rt, reg] + ',');

   case _mod of
     0 : case _rm of
           4 : // [--][--]
             begin
               sib := GetByte;
               result := result+ '[' + SegMod + DecodeSib (sib, r32) + (* '+' + gRegisters [r32,reg] + *) ']'
             end;
           5 :
             begin
               dw := GetDWORD;
               result := FormatDWORD (dw, ptrType (rt) + '[',']');
             end
           else
             result := result + '[' + SegMod + string (gRegisters [r32,_rm]) + ']';
         end;

     1 :
       begin
         case _rm of
           4 : // disp8 [--][--]
             begin
               sib := GetByte;
               result := result+ 'dword ptr [' + SegMod + DecodeSib (sib, r32) + '+' + IntToHex (GetByte, 2) + ']'
             end;
           else
             begin
               sib := GetByte;
               if sib = 0 then
                 result := result + '[' + SegMod + string (gRegisters [r32, _rm]) + ']'
               else
                 result := result + '[' + SegMod + string (gRegisters [r32, _rm]) + '+' + IntToHex (sib, 2) + ']';
             end
         end
       end;

     2 :
       begin
         case _rm of
           4 : // disp32 [--][--]
             begin
               sib := GetByte;
               result := result+ 'dword ptr [' + DecodeSib (sib, r32) + '+' + IntToHex (GetDWORD, 8) + ']'
             end;
           else
             begin
               dw := GetDWORD;
               result := result + '[' + string (gRegisters [r32, _rm]) + '+' + IntToHex (dw, 8) + ']';
             end
         end
       end;

     3 :
       begin
         if hasReg then
           result := string (gRegisters [rt, reg]) + ',';
         result := result + string (gRegisters [rt, _rm])
       end;
   end
  end;

  function GetOperand : byte;
  begin
    if not alreadyHasOperand then
      Operand := GetByte;
    result := Operand;
  end;

begin
  result := '';
  case optype of
    None: ;
    EbGb:
      begin
        EmitMODRM (GetOperand, r8, false);
        result := result + ',' + string (gRegisters [r8, (Operand shr 3) and 7]);
      end;

    EvGv:
      begin
        EmitMODRM (GetOperand, r32, false);
        result := result + ',' + string (gRegisters [r32, (Operand shr 3) and 7]);
      end;
    GbEb:
      begin
        EmitMODRM (GetOperand, r8, true);
//        result := gRegisters [r8, (Operand shr 3) and 7] + ',' + result;
      end;
    GvEv:
      begin
        EmitMODRM (GetOperand, r32, true);
//          result := gRegisters [r32, (Operand shr 3) and 7] + ',' + result;
      end;
    GvEb :
      begin
        EmitMODRM (GetOperand, r8, true);
//          result := gRegisters [r32, (Operand shr 3) and 7] + ',' + result;
      end;

    Eblb:
      begin
        EmitMODRM (GetOperand, r8, false);
        result := result + ',' + IntToHex (GetByte, 2);
      end;
    Eblv:
      begin
        EmitMODRM (GetOperand, r32, false);
        result := result + ',' + IntToHex (GetByte, 2);
      end;
    Evlv:
      begin
        EmitMODRM (GetOperand, r32, false);
        result := result + ',' + IntToHex (GetWordOrDWORD, 8);
      end;

    AlLb: result := FormatByte (GetByte, 'AL, ', '');
    eAXlv: result := FormatDWORD (GetDWORD, 'EAX, ', '');
    ES: result := 'ES';
    CS: result := 'CS';
    DS: result := 'DS';
    SS: result := 'SS';
    eAX..eDI :
      result := string (gRegisters [r32, Integer (opType) - Integer (eAX)]);
    PUSHAD: ;
    POPAD: ;
    GvMa: ;
    EwGw: ;
    FS: result := 'FS';
    GS: result := 'GS';
    lv: result := FormatDWORD (GetDWORD, '', '');
    GvEvlv: ;
    lb: result := FormatByte (GetByte, '', '');
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

    eAXCX : result := 'EAX, ECX';
    eAXDX : result := 'EAX, EDX';
    eAXBX : result := 'EAX, EBX';
    eAXSP : result := 'EAX, ESP';
    eAXBP : result := 'EAX, EBP';
    eAXSI : result := 'EAX, ESI';
    eAXDI : result := 'EAX, EDI';
    Ap :;
    Fv :;   // arg to pushf/popf - = none

    AlOb : result := 'al, dword ptr [' + IntToHex (GetDWORD, 8) + ']';
    eAXOv : result := 'eax, dword ptr [' + IntToHex (GetDWORD, 8) + ']';

    ObAL: result := '[' + IntToHex (GetDWORD, 8) + '], al';
    OvEAX:  result := '[' + IntToHex (GetDWORD, 8) + '], eax';

    XbYb:; // arg to MOVSB
    XwYw:; // arg to MOVSW
    YbAl,  //        STOSB
    YveAX, //        STOSW
    AlXb,  //        LODSB
    eAXXv, //        LODSW
    AlYb,  //        CMPSB
    eAXYv, //        CMPSW

    mal : result := FormatByte (GetByte, 'AL, ', '');
    mcl : result := FormatByte (GetByte, 'CL, ', '');
    mdl : result := FormatByte (GetByte, 'DL, ', '');
    mbl : result := FormatByte (GetByte, 'BL, ', '');
    mah : result := FormatByte (GetByte, 'AH, ', '');
    mch : result := FormatByte (GetByte, 'CH, ', '');
    mdh : result := FormatByte (GetByte, 'DH, ', '');
    mbh : result := FormatByte (GetByte, 'BH, ', '');

    meax : result := FormatDWORD (GetDWORD, 'EAX, ', '');
    mecx : result := FormatDWORD (GetDWORD, 'ECX, ', '');
    medx : result := FormatDWORD (GetDWORD, 'EDX, ', '');
    mebx : result := FormatDWORD (GetDWORD, 'EBX, ', '');
    mesp : result := FormatDWORD (GetDWORD, 'ESP, ', '');
    mebp : result := FormatDWORD (GetDWORD, 'EBP, ', '');
    mesi : result := FormatDWORD (GetDWORD, 'ESI, ', '');
    medi : result := FormatDWORD (GetDWORD, 'EDI, ', '');

    lw : result := FormatWord (GetWord, '', '');
    GvMp :;
    LwLb :;

    jb :
      begin
        i := ShortInt (GetByte);
        result := IntToHex (Integer (fP) - Integer (fMem) + i, 2);

      end;

    eaxlb :;
    lbal :;
    lbeax :;

    jv :
      begin
        i := Integer (GetDWORD);
        result := IntToHex (Integer (fP) - Integer (fMem) + i, 2);
      end;
    aldx : result := 'al, dx';
    dxal : result := 'dx, al';
    dxeax: result := 'dx, eax';
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
  ReallocMem (fMem, 0);

  inherited;
end;

procedure TDisassembler.DecodeGroup5 (var st, arg : string);
var
  b, b1 : byte;
begin
  if fLeft < 1 then
    raise Exception.Create('End of data');

  b := fP^;
  Inc (fP);
  Dec (fLeft);

  b1 := b shr 3 and 7;
  b := b and $c7;

  case b1 of
    $0 : begin st := 'inc'; arg := DecodeOp (Ev, true, b); end;
    $1 : begin st := 'dec'; arg := DecodeOp (Ev, true, b); end;
    $2 : begin st := 'call'; arg := DecodeOp (Ev, true, b); end;
    $3 : begin st := 'call'; arg := DecodeOp (Ep, true, b); end;
    $4 : begin st := 'jmp'; arg := DecodeOp (Ev, true, b); end;
    $5 : begin st := 'jmp'; arg := DecodeOp (Ep, true, b); end;
    $6 : begin st := 'push'; arg := DecodeOp (Ev, true, b); end;
  end
end;

procedure TDisassembler.ApplyRelocs;
var
  p : PSectionReloc;
  i (* , va *) : Integer;
//  symbol : TSymbol;
begin
  p := fRelocs;

  for i:= 0 to fRelocCount - 1 do
  begin
//    va := p^.virtualAddress;
//    symbol := TSymbol (fSymbols [p^.symbolTableIndex]);

    case p^._type of
      IMAGE_REL_I386_ABSOLUTE :;
      IMAGE_REL_I386_DIR32 :;
      IMAGE_REL_I386_DIR32NB :;
      IMAGE_REL_I386_REL32 :;
      else
        raise Exception.Create ('Unsupported relocation type');
    end;

    Inc (p);
  end
end;

constructor TDisassembler.Create(AMem: PByte; ALen: Integer; ARelocs : PSectionReloc; ARelocCount : Integer; ASymbols : TList);
begin
  ReallocMem (fMem, ALen);
  Move (AMem^, fMem^, ALen);
  fLen := ALen;
  fRelocs := ARelocs;
  fRelocCount := ARelocCount;
  fSymbols := ASymbols;

  ApplyRelocs;
end;

procedure TDisassembler.DecodeGroup1 (optype : Ti386OpType; var st, arg : string);
var
  b, b1 : byte;
begin
  if fLeft < 1 then
    raise Exception.Create('End of data');

  b := fP^;
  Inc (fP);
  Dec (fLeft);

  b1 := b shr 3 and 7;
  b := b and $c7;

  case b1 of
    $0 : begin st := 'add'; arg := DecodeOp (optype, true, b); end;
    $1 : begin st := 'or'; arg := DecodeOp (optype, true, b); end;
    $2 : begin st := 'adc'; arg := DecodeOp (optype, true, b); end;
    $3 : begin st := 'sbb'; arg := DecodeOp (optype, true, b); end;
    $4 : begin st := 'and'; arg := DecodeOp (optype, true, b); end;
    $5 : begin st := 'sub'; arg := DecodeOp (optype, true, b); end;
    $6 : begin st := 'xor'; arg := DecodeOp (optype, true, b); end;
    $7 : begin st := 'cmp'; arg := DecodeOp (optype, true, b); end;
  end
end;

procedure TDisassembler.Disassemble(s: TStream);
var
  base, nb : Integer;
  op, param : string;
begin
  base := 0;
  while base < fLen do
  begin
    nb := Disassemble (base, op, param);
    if nb = 0 then
      break;

    op := Format ('%8.8d'#9'%-8.8s'#9'%s'#13#10, [base, op, param]);
    s.Write(op [1], Length (op) * sizeof (char));

    Inc (base, nb);
  end
end;

function TDisassembler.Disassemble(base : Integer; var op, param : string) : Integer;
var
  ost : string;
  optxt : string;
  opcode : Ti386Opcode;
  baseP : PByte;

label
  1;

begin
  try
    fP := fMem;
    Inc (fP, base);
    baseP := fp;

    fLeft := fLen;
    Dec (fLeft, base);

    fSeg := srDS;
    fOpSize := os32;

1:  if fLeft > 0 then
    begin
      opcode := gi386OpCodes [fP^];
      case opcode.optype of
        i2Byte :
          begin
            Inc (fP);
            Dec (fLeft);
            opcode := g2Bytei386OpCodes [fP^]
          end;
        iES,
        iCS,
        iSS,
        iDS,
        iFS,
        iGS :
          begin
            case opcode.optype of
              iES : fSeg := srES;
              iCS : fSeg := srCS;
              iSS : fSeg := srSS;
              iDS : fSeg := srDS;
              iFS : fSeg := srFS;
              iGS : fSeg := srGS;
            end;
            Inc (fp);
            Dec (fLeft);
            goto 1;
          end;

        oSize,
        aSize :
          begin
            case opcode.optype of
              oSize : fOpSize := os16
            end;
            Inc (fp);
            Dec (fLeft);
            goto 1;
          end;

      end;

      Inc (fP);
      Dec (fLeft);

      with opcode do
      begin

        if txt [1] = '~' then
        case txt [2] of
          '1' : DecodeGroup1 (optype, ost, optxt);
          '4' : ;
          '5' : DecodeGroup5 (ost, optxt)
        end
        else
        begin
          ost := string (txt);
          optxt := DecodeOp (optype, false, 0);
        end
      end;

      op := ost;
      param := optxt;
      result := Integer (fP) - Integer (baseP);
    end
    else
      result := 0;
  except
    result := 0
  end;
end;

procedure TDisassembler.SizeGroup1(optype: Ti386OpType);
var
  b : byte;
begin
  if fLeft < 1 then
    raise Exception.Create('End of data');

  b := fP^;
  Inc (fP);
  Dec (fLeft);

  b := b and $c7;

  DecodeOp (optype, true, b)
end;

procedure TDisassembler.SizeGroup5;
begin
  SizeGroup1 (Ev)
end;

function TDisassembler.SizeOfInstruction (base: Integer): Integer;
var
  opcode : Ti386Opcode;
  baseP : PByte;

label
    1;

begin
  try
    fP := fMem;
    Inc (fP, base);
    baseP := fp;

    fLeft := fLen;
    Dec (fLeft, base);

    fSeg := srDS;
    fOpSize := os32;

1:  if fLeft > 0 then
    begin
      opcode := gi386OpCodes [fP^];

      case opcode.optype of
        i2Byte :
          begin
            Inc (fP);
            Dec (fLeft);
            opcode := g2Bytei386OpCodes [fP^]
          end;

        iES,
        iCS,
        iSS,
        iDS,
        iFS,
        iGS :
          begin
            case opcode.optype of
              iES : fSeg := srES;
              iCS : fSeg := srCS;
              iSS : fSeg := srSS;
              iDS : fSeg := srDS;
              iFS : fSeg := srFS;
              iGS : fSeg := srGS;
            end;
            Inc (fp);
            Dec (fLeft);
            goto 1;
          end;

        oSize,
        aSize :
          begin
            case opcode.optype of
              oSize : fOpSize := os16
            end;
            Inc (fp);
            Dec (fLeft);
            goto 1;
          end;


      end;

      Inc (fP);
      Dec (fLeft);

      with opcode do
      begin

        if txt [1] = '~' then
        case txt [2] of
          '1' : SizeGroup1 (optype);
          '4' : ;
          '5' : SizeGroup5
        end
        else
          DecodeOp (optype, false, 0);
      end;

      result := Integer (fP) - Integer (baseP);
    end
    else
      result := 0;
  except
    result := 0
  end;
end;

procedure TDisassembler.GetLineMap(lineMap: TList);
var
  base, nb : Integer;
  p : PByte;
begin
  lineMap.Clear;
  base := 0;
  while base < fLen do
  begin
    nb := SizeOfInstruction (base);
    if nb = 0 then
      break;

    p := fMem;
    Inc (p, base);
    lineMap.Add(p);
    Inc (base, nb)
  end
end;

end.
