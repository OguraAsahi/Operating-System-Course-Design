; ==========================================
; pmtest9.asm
; 编译方法：nasm pmtest9.asm -o pmtest9.com
; ==========================================

%include	"pm.inc"	; 常量, 宏, 以及一些说明

PageDirBase0		equ	200000h	; 页目录开始地址:	2M
PageTblBase0		equ	201000h	; 页表开始地址:		2M +  4K
PageDirBase1		equ	210000h	; 页目录开始地址:	2M + 64K
PageTblBase1		equ	211000h	; 页表开始地址:		2M + 64K + 4K

LinearAddrDemo	equ	00401000h

ProcPagingDemo	equ	00301000h

org	0100h
	jmp	LABEL_BEGIN

[SECTION .gdt]
; GDT
;                                         段基址,       段界限     , 属性
LABEL_GDT:		Descriptor	       0,                 0, 0				; 空描述符
LABEL_DESC_NORMAL:	Descriptor	       0,            0ffffh, DA_DRW			; Normal 描述符
LABEL_DESC_FLAT_C:	Descriptor             0,           0fffffh, DA_CR | DA_32 | DA_LIMIT_4K; 0 ~ 4G
LABEL_DESC_FLAT_RW:	Descriptor             0,           0fffffh, DA_DRW | DA_LIMIT_4K	; 0 ~ 4G
LABEL_DESC_CODE32:	Descriptor	       0,  SegCode32Len - 1, DA_CR | DA_32		; 非一致代码段, 32
LABEL_DESC_CODE16:	Descriptor	       0,            0ffffh, DA_C			; 非一致代码段, 16
LABEL_DESC_DATA:	Descriptor	       0,	DataLen - 1, DA_DRW			; Data
LABEL_DESC_STACK:	Descriptor	       0,        TopOfStack, DA_DRWA | DA_32		; Stack, 32 位
LABEL_DESC_VIDEO:	Descriptor	 0B8000h,            0ffffh, DA_DRW			; 显存首地址

;-------------------------------------------------------------------------------------
LABEL_DESC_LDT0:        Descriptor 		0,          LDT0Len-1, DA_LDT		 	;LDT0
LABEL_DESC_LDT1:        Descriptor		0,          LDT1Len-1, DA_LDT		   	;LDT1
LABEL_DESC_TSS0:        Descriptor		0,          TSS0Len-1, DA_386TSS	   		;TSS0
LABEL_DESC_TSS1:        Descriptor 		0,          TSS1Len-1, DA_386TSS	   		;TSS1

LABEL_DESC_STACK0:      Descriptor 		0,        TopOfStack0, DA_DRWA+DA_32	   	;Stack0,32
LABEL_DESC_STACK1:      Descriptor 		0,        TopOfStack1, DA_DRWA+DA_32	   	;Stack1,32
;-------------------------------------------------------------------------------------
; GDT 结束

GdtLen		equ	$ - LABEL_GDT	; GDT长度
GdtPtr		dw	GdtLen - 1	; GDT界限
		dd	0		; GDT基地址

; GDT 选择子
SelectorNormal		equ	LABEL_DESC_NORMAL	- LABEL_GDT
SelectorFlatC		equ	LABEL_DESC_FLAT_C	- LABEL_GDT
SelectorFlatRW		equ	LABEL_DESC_FLAT_RW	- LABEL_GDT
SelectorCode32		equ	LABEL_DESC_CODE32	- LABEL_GDT
SelectorCode16		equ	LABEL_DESC_CODE16	- LABEL_GDT
SelectorData		equ	LABEL_DESC_DATA		- LABEL_GDT
SelectorStack		equ	LABEL_DESC_STACK	- LABEL_GDT
SelectorVideo		equ	LABEL_DESC_VIDEO	- LABEL_GDT

;-------------------------------------------------------------------------------------
SelectorLDT0		equ	LABEL_DESC_LDT0		- LABEL_GDT
SelectorLDT1		equ	LABEL_DESC_LDT1		- LABEL_GDT
SelectorTSS0		equ	LABEL_DESC_TSS0		- LABEL_GDT
SelectorTSS1		equ	LABEL_DESC_TSS1		- LABEL_GDT

SelectorStack0		equ	LABEL_DESC_STACK0	- LABEL_GDT
SelectorStack1		equ	LABEL_DESC_STACK1	- LABEL_GDT
;-------------------------------------------------------------------------------------
; END of [SECTION .gdt]

[SECTION .data1]	 ; 数据段
ALIGN	32
[BITS	32]
LABEL_DATA:
; 实模式下使用这些符号
; 字符串
_szPMMessage:			db	"In Protect Mode now. ^-^", 0Ah, 0Ah, 0	; 进入保护模式后显示此字符串
_szMemChkTitle:			db	"BaseAddrL BaseAddrH LengthLow LengthHigh   Type", 0Ah, 0	; 进入保护模式后显示此字符串
_szRAMSize			db	"RAM size:", 0
_szReturn			db	0Ah, 0
; 变量
_wSPValueInRealMode		dw	0
_dwMCRNumber:			dd	0	; Memory Check Result
_dwDispPos:			dd	(80 * 6 + 0) * 2	; 屏幕第 6 行, 第 0 列。
_dwMemSize:			dd	0
_ARDStruct:			; Address Range Descriptor Structure
	_dwBaseAddrLow:		dd	0
	_dwBaseAddrHigh:	dd	0
	_dwLengthLow:		dd	0
	_dwLengthHigh:		dd	0
	_dwType:		dd	0
_PageTableNumber:		dd	0
_SavedIDTR:			dd	0	; 用于保存 IDTR
				dd	0
_SavedIMREG:			db	0	; 中断屏蔽寄存器值
_MemChkBuf:	times	256	db	0

; 保护模式下使用这些符号
szPMMessage		equ	_szPMMessage	- $$
szMemChkTitle		equ	_szMemChkTitle	- $$
szRAMSize		equ	_szRAMSize	- $$
szReturn		equ	_szReturn	- $$
dwDispPos		equ	_dwDispPos	- $$
dwMemSize		equ	_dwMemSize	- $$
dwMCRNumber		equ	_dwMCRNumber	- $$
ARDStruct		equ	_ARDStruct	- $$
	dwBaseAddrLow	equ	_dwBaseAddrLow	- $$
	dwBaseAddrHigh	equ	_dwBaseAddrHigh	- $$
	dwLengthLow	equ	_dwLengthLow	- $$
	dwLengthHigh	equ	_dwLengthHigh	- $$
	dwType		equ	_dwType		- $$
MemChkBuf		equ	_MemChkBuf	- $$
SavedIDTR		equ	_SavedIDTR	- $$
SavedIMREG		equ	_SavedIMREG	- $$
PageTableNumber		equ	_PageTableNumber- $$

DataLen			equ	$ - LABEL_DATA
; END of [SECTION .data1]


; IDT
[SECTION .idt]
ALIGN	32
[BITS	32]
LABEL_IDT:
; 门                        目标选择子,            偏移, DCount, 属性
%rep 32
		Gate	SelectorCode32, SpuriousHandler,      0, DA_386IGate
%endrep
;----------------------------------------------------------------------------------
.020h:		Gate	SelectorCode32,    ClockHandler,      0, DA_386IGate
;----------------------------------------------------------------------------------
%rep 95
		Gate	SelectorCode32, SpuriousHandler,      0, DA_386IGate
%endrep
.080h:		Gate	SelectorCode32,  UserIntHandler,      0, DA_386IGate

IdtLen		equ	$ - LABEL_IDT
IdtPtr		dw	IdtLen - 1	; 段界限
		dd	0		; 基地址
; END of [SECTION .idt]


; 全局堆栈段
[SECTION .gs]
ALIGN	32
[BITS	32]
LABEL_STACK:
	times 512 db 0

TopOfStack	equ	$ - LABEL_STACK - 1

; END of [SECTION .gs]



; 堆栈段stack0-------------------TASK0
[SECTION .s0]
ALIGN	32
[BITS	32]
LABEL_STACK0:
	times 512 db 0
TopOfStack0	equ	$ - LABEL_STACK0 - 1
; END of [SECTION .s0]


; 堆栈段stack1--------------------TASK1
[SECTION .s1]
ALIGN	32
[BITS	32]
LABEL_STACK1:
	times 512 db 0
TopOfStack1	equ	$ - LABEL_STACK1 - 1
; END of [SECTION .s1]

; TSS ---------------------------------------------------------------------------------------------



[SECTION .tss0]
ALIGN	32
[BITS	32]
LABEL_TSS0:
		DD	0			; Back
		DD	TopOfStack		; 0 级堆栈
		DD	SelectorStack		; 
		DD	0			; 1 级堆栈
		DD	0			; 
		DD	0			; 2 级堆栈
		DD	0			; 
		DD	0			; CR3
		DD	0			; EIP
		DD	0			; EFLAGS
		DD	0			; EAX
		DD	0			; ECX
		DD	0			; EDX
		DD	0			; EBX
		DD	0			; ESP
		DD	0			; EBP
		DD	0			; ESI
		DD	0			; EDI
		DD	0			; ES
		DD	0			; CS
		DD	0			; SS
		DD	0			; DS
		DD	0			; FS
		DD	0			; GS
		DD	0			; LDT
		DW	0			; 调试陷阱标志
		DW	$ - LABEL_TSS0 + 2	; I/O位图基址
		DB	0ffh			; I/O位图结束标志
TSS0Len		equ	$ - LABEL_TSS0
; TSS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


; TSS ---------------------------------------------------------------------------------------------
[SECTION .tss1]
ALIGN	32
[BITS	32]
LABEL_TSS1:
		DD	0			; Back
		DD	TopOfStack		; 0 级堆栈
		DD	SelectorStack		; 
		DD	0			; 1 级堆栈
		DD	0			; 
		DD	0			; 2 级堆栈
		DD	0			; 
		DD	0			; CR3
		DD	0			; EIP
		DD	0			; EFLAGS
		DD	0			; EAX
		DD	0			; ECX
		DD	0			; EDX
		DD	0			; EBX
		DD	0			; ESP
		DD	0			; EBP
		DD	0			; ESI
		DD	0			; EDI
		DD	0			; ES
		DD	0			; CS
		DD	0			; SS
		DD	0			; DS
		DD	0			; FS
		DD	0			; GS
		DD	0			; LDT
		DW	0			; 调试陷阱标志
		DW	$ - LABEL_TSS1 + 2	; I/O位图基址
		DB	0ffh			; I/O位图结束标志
TSS1Len		equ	$ - LABEL_TSS1
; TSS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


[SECTION .s16]
[BITS	16]		;代码为16位段
LABEL_BEGIN:
	mov	ax, cs
	mov	ds, ax
	mov	es, ax
	mov	ss, ax
	mov	sp, 0100h

	mov	[LABEL_GO_BACK_TO_REAL+3], ax		;ax此时是实模式的cs,将实模式cs转给返回实模式时的jump语句
	mov	[_wSPValueInRealMode], sp		;将堆栈指针sp保存在变量_wSPValueInRealMode中

	; 得到内存数
	mov	ebx, 0	; 将ebx置0，这是eax=0000E820h的15h号中断段的必要条件
	mov	di, _MemChkBuf	;让di指向内存信息缓冲区的首址，用于存放中断输出的内存信息
.loop:
	mov	eax, 0E820h	; 将eax置为0000E820h
	mov	ecx, 20			; 表示读取内存信息后BIOS所存放的字节数，但通常无论ecx多大BIOS均填充20字节
	mov	edx, 0534D4150h	; 'SMAP', BIOS会利用此标志对系统映像信息进行校验并存放到es:di所指向的空间中
	int	15h			; 调用15h中断
	jc	LABEL_MEM_CHK_FAIL	; 当cf被置位时就会跳转结束循环
	add	di, 20			; 让es:di指向缓冲区中的下20个字节继续存放内存信息
	inc	dword [_dwMCRNumber]	; 让_dwMCRNumber增加一，这个变量记录了循环次数，也就是地址范围描述符的个数
	cmp	ebx, 0		; 如果ebx为0则跳出循环，否则继续读取
	jne	.loop
	jmp	LABEL_MEM_CHK_OK
LABEL_MEM_CHK_FAIL:
	mov	dword [_dwMCRNumber], 0 ; 如果是cf被置位引起的循环调出，则将MCR个数置为0
LABEL_MEM_CHK_OK:

	; 初始化 16 位代码段描述符
	mov	ax, cs
	movzx	eax, ax
	shl	eax, 4						;将段值右移四位
	add	eax, LABEL_SEG_CODE16		;eax加上16位代码段的偏移最后得到物理地址
	mov	word [LABEL_DESC_CODE16 + 2], ax	;将物理地址分为3段放入描述符中相应的位置
	shr	eax, 16
	mov	byte [LABEL_DESC_CODE16 + 4], al
	mov	byte [LABEL_DESC_CODE16 + 7], ah	;16位代码段描述符初始化完成

	; 初始化 32 位代码段描述符
	xor	eax, eax		;将寄存器eax置0
	mov	ax, cs
	shl	eax, 4						;将段值右移四位
	add	eax, LABEL_SEG_CODE32			;eax加上数据段的偏移最后得到物理地址
	mov	word [LABEL_DESC_CODE32 + 2], ax		;将物理地址分为3段放入描述符中相应的位置
	shr	eax, 16
	mov	byte [LABEL_DESC_CODE32 + 4], al
	mov	byte [LABEL_DESC_CODE32 + 7], ah		;数据段描述符初始化完成

	; 初始化数据段描述符
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_DATA
	mov	word [LABEL_DESC_DATA + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_DATA + 4], al
	mov	byte [LABEL_DESC_DATA + 7], ah

	; 初始化堆栈段描述符
	xor	eax, eax		;将寄存器置0
	mov	ax, ds
	shl	eax, 4						;将段值右移四位
	add	eax, LABEL_STACK			;eax加上堆栈段的偏移最后得到物理地址
	mov	word [LABEL_DESC_STACK + 2], ax		;将物理地址分为3段放入描述符中相应的位置
	shr	eax, 16
	mov	byte [LABEL_DESC_STACK + 4], al
	mov	byte [LABEL_DESC_STACK + 7], ah	;堆栈段描述符初始化完成
;---------------------------------------------------------------------------------------------
	; 初始化堆栈段描述符			LABEL_STACK0
	xor	eax, eax		;将寄存器置0
	mov	ax, ds
	shl	eax, 4						;将段值右移四位
	add	eax, LABEL_STACK0			;eax加上堆栈段的偏移最后得到物理地址
	mov	word [LABEL_DESC_STACK0 + 2], ax	;将物理地址分为3段放入描述符中相应的位置
	shr	eax, 16
	mov	byte [LABEL_DESC_STACK0 + 4], al
	mov	byte [LABEL_DESC_STACK0 + 7], ah

	mov	byte [LABEL_DESC_STACK1 + 7], ah;堆栈段描述符初始化完成

	; 初始化 LDT 在 GDT 中的描述符	LABEL_STACK0
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_LDT0
	mov	word [LABEL_DESC_LDT0 + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_LDT0 + 4], al
	mov	byte [LABEL_DESC_LDT0 + 7], ah

	; 初始化 LDT 中的描述符			LABEL_STACK0
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_CODE_A
	mov	word [LABEL_LDT0_DESC_CODEA + 2], ax
	shr	eax, 16
	mov	byte [LABEL_LDT0_DESC_CODEA + 4], al
	mov	byte [LABEL_LDT0_DESC_CODEA + 7], ah
	
	; 初始化 TSS 描述符				LABEL_STACK0
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_TSS0
	mov	word [LABEL_DESC_TSS0 + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_TSS0 + 4], al
	mov	byte [LABEL_DESC_TSS0 + 7], ah
	
;---------------------------------------------------------------------------------------------
	
	; 初始化堆栈段描述符			LABEL_STACK1
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_STACK1
	mov	word [LABEL_DESC_STACK1 + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_STACK1 + 4], al
	
	; 初始化 LDT 在 GDT 中的描述符	LABEL_STACK1
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_LDT1
	mov	word [LABEL_DESC_LDT1 + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_LDT1 + 4], al
	mov	byte [LABEL_DESC_LDT1 + 7], ah

	; 初始化 LDT 中的描述符			LABEL_STACK1
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_CODE_B
	mov	word [LABEL_LDT1_DESC_CODEB + 2], ax
	shr	eax, 16
	mov	byte [LABEL_LDT1_DESC_CODEB + 4], al
	mov	byte [LABEL_LDT1_DESC_CODEB + 7], ah

	; 初始化 TSS 描述符				LABEL_STACK1
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_TSS1
	mov	word [LABEL_DESC_TSS1 + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_TSS1 + 4], al
	mov	byte [LABEL_DESC_TSS1 + 7], ah

;---------------------------------------------------------------------------------------------

	; 为加载 GDTR 作准备
	xor	eax, eax		;将寄存器置0
	mov	ax, ds
	shl	eax, 4					;将段值右移4位
	add	eax, LABEL_GDT			;eax加上堆栈段的偏移最后得到物理地址
	mov	dword [GdtPtr + 2], eax	;gdt基址存入gdtr

	; 为加载 IDTR 作准备
	xor	eax, eax		;将寄存器置0
	mov	ax, ds
	shl	eax, 4					;将段值右移4位
	add	eax, LABEL_IDT		    ;eax加上堆栈段的偏移最后得到物理地址
	mov	dword [IdtPtr + 2], eax	;gdt基址存入gdtr

	; 保存 IDTR
	sidt	[_SavedIDTR]

	; 保存中断屏蔽寄存器(IMREG)值
	in	al, 21h
	mov	[_SavedIMREG], al

	; 加载 GDTR
	lgdt	[GdtPtr]

	; 关中断
	;cli

	; 加载 IDTR
	lidt	[IdtPtr]

	; 打开地址线A20
	in	al, 92h
	or	al, 00000010b
	out	92h, al

	; 准备切换到保护模式
	mov	eax, cr0
	or	eax, 1		;将cr0的PE位置1
	mov	cr0, eax

	; 真正进入保护模式
	jmp	dword SelectorCode32:0	; 将SelectorCode32装入cs, 程序跳转到Code32Selector:0 处

;---------------------------------------------------------------------------------------------

LABEL_REAL_ENTRY:		; 从保护模式跳回到实模式就到了这里
	mov	ax, cs		;恢复各个寄存器的值
	mov	ds, ax
	mov	es, ax
	mov	ss, ax
	mov	sp, [_wSPValueInRealMode]	;恢复栈指针

	lidt	[_SavedIDTR]	; 恢复 IDTR 的原值

	mov	al, [_SavedIMREG]	; 恢复中断屏蔽寄存器(IMREG)的原值
	out	21h, al			; 

	in	al, 92h		; 
	and	al, 11111101b	;  关闭 A20 地址线
	out	92h, al		; 

	sti			; 开中断

	mov	ax, 4c00h	; 
	int	21h		; 回到 DOS
; END of [SECTION .s16]


[SECTION .s32]; 32 位代码段. 由实模式跳入.
[BITS	32]

LABEL_SEG_CODE32:
	mov	ax, SelectorData
	mov	ds, ax			; 数据段选择子
	mov	es, ax
	mov	ax, SelectorVideo
	mov	gs, ax			; 视频段(屏幕缓冲区)选择子

	mov	ax, SelectorStack
	mov	ss, ax			; 堆栈段选择子
	mov	esp, TopOfStack ;设置新堆栈栈顶指针

	call	Init8259A   ;---------------

	int	080h
	sti
	;jmp	$

	; 下面显示一个字符串
	push	szPMMessage		;显示进入保护模式, 将_szPMMessage地址入栈,szPMMessage在数据段中定义
	call	DispStr         ;显示"In Protect Mode now. ^-^"
	add	esp, 4              ;将栈指针下移，不使用pop就将字符串地址占用的空间释放

	call	DispMemSize		; 显示内存信息

	jmp	$

	call	SetRealmode8259A
	; 到此停止
	jmp	SelectorCode16:0	;跳转去CODE16段，为返回实模式进行准备工作

; Init8259A ---------------------------------------------------------------------------------------------
Init8259A:
	mov	al, 011h
	out	020h, al	; 主8259, ICW1.
	call	io_delay

	out	0A0h, al	; 从8259, ICW1.
	call	io_delay

	mov	al, 020h	; IRQ0 对应中断向量 0x20
	out	021h, al	; 主8259, ICW2.
	call	io_delay

	mov	al, 028h	; IRQ8 对应中断向量 0x28
	out	0A1h, al	; 从8259, ICW2.
	call	io_delay

	mov	al, 004h	; IR2 对应从8259
	out	021h, al	; 主8259, ICW3.
	call	io_delay

	mov	al, 002h	; 对应主8259的 IR2
	out	0A1h, al	; 从8259, ICW3.
	call	io_delay

	mov	al, 001h
	out	021h, al	; 主8259, ICW4.
	call	io_delay

	out	0A1h, al	; 从8259, ICW4.
	call	io_delay

	;mov	al, 11111111b	; 屏蔽主8259所有中断
	mov	al, 11111110b	; 仅仅开启定时器中断
	out	021h, al	; 主8259, OCW1.
	call	io_delay

	mov	al, 11111111b	; 屏蔽从8259所有中断
	out	0A1h, al	; 从8259, OCW1.
	call	io_delay

	ret
; Init8259A ---------------------------------------------------------------------------------------------


; SetRealmode8259A ---------------------------------------------------------------------------------------------
SetRealmode8259A:
	mov	ax, SelectorData
	mov	fs, ax

	mov	al, 017h
	out	020h, al	; 主8259, ICW1.
	call	io_delay

	mov	al, 008h	; IRQ0 对应中断向量 0x8
	out	021h, al	; 主8259, ICW2.
	call	io_delay

	mov	al, 001h
	out	021h, al	; 主8259, ICW4.
	call	io_delay

	mov	al, [fs:SavedIMREG]	; 恢复中断屏蔽寄存器(IMREG)的原值
	out	021h, al		; 
	call	io_delay

	ret
; SetRealmode8259A ---------------------------------------------------------------------------------------------

io_delay:
	nop
	nop
	nop
	nop
	ret

; int handler ---------------------------------------------------------------
_ClockHandler:
ClockHandler	equ	_ClockHandler - $$
;--------------------------------------------------------------------------
	;inc	byte [gs:((80 * 0 + 70) * 2)]	; 屏幕第 0 行, 第 70 列。
	mov byte [gs:((80 * 0 + 70) * 2)],'_'
	inc	cx

	push	ds
	push	edx
	push	ecx
	push	ebx
	push	eax

	mov	ax, SelectorData
	mov	ds, ax			; 数据段选择子
	mov	ah, 0Ch				; 0000: 黑底    1100: 红字

	;mov bl,byte [gs:((80 * 0 + 70) * 2)]
	;and bl,1
	mov bx,cx
	and bx,1
	mov ax,0
	cmp bx,ax
	je HUSTDisplay
	jmp MRSUDisplay
	
MRSUDisplay:
	mov	ax, SelectorLDT0
	lldt	ax

	jmp	SelectorLDT0CodeA:0	; 跳入局部任务，HUST。
	
	jmp	Final
	
HUSTDisplay:

	mov	ax, SelectorLDT1
	lldt	ax

	jmp	SelectorLDT1CodeB:0	; 跳入局部任务，MRSU

Final:
	mov	al, 20h
	out	20h, al				; 发送 EOI
	pop	eax
	pop	ebx
	pop	ecx
	pop	edx
	pop 	ds
	iretd
;--------------------------------------------------------------------------

_UserIntHandler:
UserIntHandler	equ	_UserIntHandler - $$
	mov	ah, 0Ch				; 0000: 黑底    1100: 红字
	mov	al, 'I'
	mov	[gs:((80 * 0 + 70) * 2)], ax	; 屏幕第 0 行, 第 70 列。
	iretd

_SpuriousHandler:
SpuriousHandler	equ	_SpuriousHandler - $$
	mov	ah, 0Ch				; 0000: 黑底    1100: 红字
	mov	al, '!'
	mov	[gs:((80 * 0 + 75) * 2)], ax	; 屏幕第 0 行, 第 75 列。
	jmp	$
	iretd
; ---------------------------------------------------------------------------

; 启动分页机制 --------------------------------------------------------------
SetupPaging:
	; 根据内存大小计算应初始化多少PDE以及多少页表
	xor	edx, edx	; 将edx置0
	mov	eax, [dwMemSize]; 将在实模式下读取的内存大小赋值给eax
	mov	ebx, 400000h	; 400000h = 4M = 4096 * 1024, 一个页表对应的内存大小
	div	ebx             ; 执行eax/ebx，并将商存放在eax中，余数存放在edx中，此时eax中的值即为所需要的页表数（减一）
	mov	ecx, eax	    ; 此时 ecx 为页表的个数，也即 PDE 应该的个数
	test	edx, edx	; 检测前面除法得到的余数是否为0
	jz	.no_remainder
	inc	ecx		; 如果余数不为 0 就需增加一个页表
.no_remainder:
	mov	[PageTableNumber], ecx	; 暂存页表个数

	; 为简化处理, 所有线性地址对应相等的物理地址. 并且不考虑内存空洞.

	; 首先初始化页目录
	mov	ax, SelectorFlatRW	;让es指向Flat段，并进行读写操作
	mov	es, ax
	mov	edi, PageDirBase0	; 此段首地址为 PageDirBase
	xor	eax, eax            ; 将eax置0
	mov	eax, PageTblBase0 | PG_P  | PG_USU | PG_RWW ; 向eax中写入页目录表项的属性
.1:
	stosd				; 该指令将eax中的信息写入es:edi指向的内存，同时将edi增加4个字节的大小，由上文可知，这里的内存也就是页目录
	add	eax, 4096		; 为了简化, 所有页表在内存中是连续的. 增加4096后即为下一个页面的起始地址
	loop	.1			; 循环赋值，开始循环时时ecx中的值正好是页表的个数

	; 再初始化所有页表
	mov	eax, [PageTableNumber]	; 页表个数
	mov	ebx, 1024		; 每个页表 1024 个 PTE
	mul	ebx			    ; 相乘后得到PTE的总个数
	mov	ecx, eax		; PTE个数 = 页表个数 * 1024
	mov	edi, PageTblBase0	; 此段首地址为 PageTblBase
	xor	eax, eax		; eax置0
	mov	eax, PG_P  | PG_USU | PG_RWW	; 向eax中写入PTE的属性
.2:
	stosd				; 类似前文，初始化PTE中的信息
	add	eax, 4096		; 每一页指向 4K 的空间
	loop	.2			; 循环赋值，开始循环时时ecx中的值正好为PTE的个数

	mov	eax, PageDirBase0	; 页目录的基址
	mov	cr3, eax		    ; 让cr3指向页目录的基址
	mov	eax, cr0		    ; 把cr0赋值给eax
	or	eax, 80000000h		; 将最高位置1.对应即为cr0的PG位
	mov	cr0, eax		    ; 再将结果赋值给cr0，完成对PG位的置位
	jmp	short .3		    ; 跳转
.3:
	nop			            ; 空指令，等待操作完成

	ret			            ; 返回
; 分页机制启动完毕 ----------------------------------------------------------


; 切换页表 ------------------------------------------------------------------
PSwitch:
	; 初始化页目录
	mov	ax, SelectorFlatRW	; 此处与SetupPaging函数中的代码相同，对页目录进行了初始化
	mov	es, ax
	mov	edi, PageDirBase1	; 此段首地址为 PageDirBase
	xor	eax, eax            ; 把eax置0
	mov	eax, PageTblBase1 | PG_P  | PG_USU | PG_RWW ; 向eax中写入页目录表项的属性
	mov	ecx, [PageTableNumber]
.1:
	stosd
	add	eax, 4096		; 为了简化, 所有页表在内存中是连续的.
	loop	.1

	; 此处与SetupPaging函数中的代码相同，再初始化了所有页表
	mov	eax, [PageTableNumber]	; 页表个数
	mov	ebx, 1024		; 每个页表 1024 个 PTE
	mul	ebx
	mov	ecx, eax		; PTE个数 = 页表个数 * 1024
	mov	edi, PageTblBase1	; 此段首地址为 PageTblBase
	xor	eax, eax
	mov	eax, PG_P  | PG_USU | PG_RWW
.2:
	stosd
	add	eax, 4096		; 每一页指向 4K 的空间
	loop	.2

	; 在此假设内存是大于 8M 的
	mov	eax, LinearAddrDemo 	; 把线性地址赋值给eax
	shr	eax, 22			        ; 右移22位，留下地址的高十位，从而得到该线性地址对应的页目录表项的编号
	mov	ebx, 4096		        ; 把4096赋值给ebx
	mul	ebx			            ; 相乘，就得到对应页表的首地址
	mov	ecx, eax		        ; 将上述结果赋给ecx
	mov	eax, LinearAddrDemo	    ; 再把线性地址赋给eax
	shr	eax, 12			        ; 右移12位
	and	eax, 03FFh	; 1111111111b (10 bits)		; 1111111111b (10 bits) 此操作后会留下原地址的12-21位
	mov	ebx, 4
	mul	ebx                     ; 乘以4即得到线性地址对应的页表表项相对于所在页表首地址的偏移
	add	eax, ecx		        ; 加上偏移，就得到对应页表表项相对于页表首地址的偏移
	add	eax, PageTblBase1	    ; 加上页表首地址，就得到对应页表表项相对于Flat段首地址的偏移
	mov	dword [es:eax], ProcBar | PG_P | PG_USU | PG_RWW	; 向对应的PTE中写入ProTask1的首地址和其他属性

	mov	eax, PageDirBase1	    ; 将页目录首地址赋值给cr3
	mov	cr3, eax
	jmp	short .3
.3:
	nop			; 空指令

	ret				; 返回
; ---------------------------------------------------------------------------

; 显示内存信息 --------------------------------------------------------------
DispMemSize:
	push	esi
	push	edi
	push	ecx

	mov	esi, MemChkBuf		;15h号中断输出的内存信息存放地址
	mov	ecx, [dwMCRNumber]	;需要显示的ARDS数量，该变量在156行被赋值
.loop:					
	mov	edx, 5			;	循环5次
	mov	edi, ARDStruct		;将ARDStruct的地址赋给edi
.1:					;
	push	dword [esi]		;将待显示整形数入栈
	call	DispInt			;依次显示BaseAddrLow，BaseAddrHigh，LengthLow，LengthHigh，Type
	pop	eax			;将eax赋值为待显示整形数
	stosd				;将eax中的待显示整型数放入ARDStruct中
	add	esi, 4			;指向下一个整型数
	dec	edx			;
	cmp	edx, 0			;判断是否已循环5次
	jnz	.1			;	未满5次，继续循环
	call	DispReturn		;循环结束，输出换行符
	cmp	dword [dwType], 1	;可用内存为1, 保留内存为2
	jne	.2			;如果是可用内存，则进行如下操作，否则跳转至622行
	mov	eax, [dwBaseAddrLow]	;
	add	eax, [dwLengthLow]	;
	cmp	eax, [dwMemSize]	;求每段可用内存的基址与长度之和
	jb	.2			;如果小于现有最大值，则跳转至622行
	mov	[dwMemSize], eax	;内存大小为所有可用内存段基址与长度之和的最大值	
.2:					;	
	loop	.loop			;循环_dwMCRNumber次
					;
	call	DispReturn		;循环结束，输出换行符
	push	szRAMSize		;
	call	DispStr			;显示"RAM size:"
	add	esp, 4			;直接更改esp，效果等于pop指令
					;
	push	dword [dwMemSize]	;
	call	DispInt			;显示内存大小
	add	esp, 4			;

	pop	ecx
	pop	edi
	pop	esi
	ret				; 返回
; ---------------------------------------------------------------------------
%include	"lib.inc"	; 库函数

SegCode32Len	equ	$ - LABEL_SEG_CODE32
; END of [SECTION .s32]


; 16 位代码段. 由 32 位代码段跳入, 跳出后到实模式
[SECTION .s16code]
ALIGN	32		;对齐伪指令,本伪指令下面的内存变量必须从下一个能被32整除的地址开始分配
[BITS	16]		;指明代码是16位段
LABEL_SEG_CODE16:
	; 跳回实模式:
	mov	ax, SelectorNormal		;将选择子装入ds,es,fs,gs,ss寄存器中
	mov	ds, ax					;这些段寄存器中的内容会根据选择子
	mov	es, ax					;更新出符合实模式要求的高速缓冲寄存器
	mov	fs, ax
	mov	gs, ax
	mov	ss, ax

	mov	eax, cr0		;将cr0的PE位置0
	and	al, 11111110b
	mov	cr0, eax

LABEL_GO_BACK_TO_REAL:		;返回实模式，程序将跳转至LABEL_REAL_ENTRY（249行）
	jmp	0:LABEL_REAL_ENTRY	; 段地址会在143行被设置成正确的值

Code16Len	equ	$ - LABEL_SEG_CODE16

; END of [SECTION .s16code]
; ---------------------------------------------------------------------------===========================================================

; LDT0
[SECTION .ldt0]
ALIGN	32
LABEL_LDT0:
;                                         段基址       段界限     ,   属性
LABEL_LDT0_DESC_CODEA:	Descriptor	       0,     CodeALen - 1,   DA_C + DA_32	; Code, 32 位

LDT0Len		equ	$ - LABEL_LDT0

; LDT 选择子
SelectorLDT0CodeA		equ				LABEL_LDT0_DESC_CODEA	- LABEL_LDT0 + SA_TIL
; END of [SECTION .ldt0]


; LDT1
[SECTION .ldt1]
ALIGN	32
LABEL_LDT1:
;                                         段基址       段界限     ,   属性
LABEL_LDT1_DESC_CODEB:	Descriptor	       0,     CodeBLen - 1,   DA_C + DA_32	; Code, 32 位

LDT1Len		equ	$ - LABEL_LDT1

; LDT 选择子
SelectorLDT1CodeB		equ				LABEL_LDT1_DESC_CODEB	- LABEL_LDT1 + SA_TIL
; END of [SECTION .ldt0]


; CodeA (LDT, 32 位代码段)
[SECTION .la]
ALIGN	32
[BITS	32]
LABEL_CODE_A:
	mov	ax, SelectorVideo
	mov	gs, ax			; 视频段选择子(目的)

	mov	ah, 0Ch				; 0000: 黑底    1100: 红字
	mov	al, 'H'
	mov	[gs:((80 * 17 + 0) * 2)], ax	; 屏幕第 17 行, 第 0 列。
	mov	al, 'U'
	mov	[gs:((80 * 17 + 1) * 2)], ax	; 屏幕第 17 行, 第 1 列。
	mov	al, 'S'
	mov	[gs:((80 * 17 + 2) * 2)], ax	; 屏幕第 17 行, 第 2 列。
	mov	al, 'T'
	mov	[gs:((80 * 17 + 3) * 2)], ax	; 屏幕第 17 行, 第 3 列。

	mov	al, 20h
	out	20h, al				; 发送 EOI
	pop	eax
	pop	ebx
	pop	ecx
	pop	edx
	pop 	ds
	iretd

CodeALen	equ	$ - LABEL_CODE_A
; END of [SECTION .la]

; CodeB (LDT, 32 位代码段)
[SECTION .lb]
ALIGN	32
[BITS	32]
LABEL_CODE_B:
	mov	ax, SelectorVideo
	mov	gs, ax			; 视频段选择子(目的)

	mov	ah, 0Ch				; 0000: 黑底    1100: 红字
	mov	al, 'M'
	mov	[gs:((80 * 17 + 0) * 2)], ax	; 屏幕第 17 行, 第 0 列。
	mov	al, 'R'
	mov	[gs:((80 * 17 + 1) * 2)], ax	; 屏幕第 17 行, 第 1 列。
	mov	al, 'S'
	mov	[gs:((80 * 17 + 2) * 2)], ax	; 屏幕第 17 行, 第 2 列。
	mov	al, 'U'
	mov	[gs:((80 * 17 + 3) * 2)], ax	; 屏幕第 17 行, 第 3 列。

	mov	al, 20h
	out	20h, al				; 发送 EOI
	pop	eax
	pop	ebx
	pop	ecx
	pop	edx
	pop 	ds
	iretd

CodeBLen	equ	$ - LABEL_CODE_B
; END of [SECTION .lb]

; ---------------------------------------------------------------------------=================================================
