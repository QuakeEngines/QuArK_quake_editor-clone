(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) QuArK Development Team

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

http://quark.sourceforge.net/ - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.2  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

}

   (* THIS IS SUPPOSED TO BE COMPILED THROUGH CCODE.PAS *)

   (* .file	"resizer.c" *)
   (* / GNU C version 2.7.2p snapshot 970522 (i386-go32-msdos) compiled by GNU C version 2.7.2p snapshot 970502. *)
   (* / options passed:  -O9 *)
   (* / options enabled:  -fdefer-pop -fomit-frame-pointer -fcse-follow-jumps *)
   (* / -fcse-skip-blocks -fexpensive-optimizations -fthread-jumps *)
   (* / -fstrength-reduce -funroll-loops -fpeephole -fforce-mem -ffunction-cse *)
   (* / -finline-functions -finline -fkeep-static-consts -fcaller-saves *)
   (* / -fpcc-struct-return -frerun-cse-after-loop -fschedule-insns2 *)
   (* / -fsjlj-exceptions -fcommon -fverbose-asm -fgnu-linker -fcompare-elim *)
   (* / -fsign-extension-elim -fjump-back -fopt-reg-use -fall-mem-givs *)
   (* / -freduce-index-givs -fpeep-spills -fsoftware-pipe -fcopy-prop *)
   (* / -flift-stores -fruntime-lift-stores -fdo-offload -fcorrect-cse-mistakes *)
   (* / -fopt-jumps-out -freplace-mem -freplace-stack-mem *)
   (* / -finterleave-stack-non-stack -fschedule-stack-reg-insns *)
   (* / -freg-reg-copy-opt -fpush-load-into-loop -fswap-for-agi -frisc *)
   (* / -frisc-const -freplace-reload-regs -frisc-mem-dest -m80387 -mhard-float *)
   (* / -mno-soft-float -mieee-fp -mfp-ret-in-387 -mschedule-prologue *)
   (* / -mcpu=pentium -march=pentium *)

   (* gcc2_compiled.: *)
@___gnu_compiled_c:
   (* .text *)
   (* .globl _Resample *)
@_Resample:
mov edx,dword [12+esp]
sub esp,dword +4224
push ebp
push edi
push esi
push ebx
test edx,edx
je @L2
mov edi,dword +-256
lea ebx,dword [144+esp]
xor ecx,ecx
mov ebp,edx
mov dword [116+esp],ebx
mov dword [24+esp],ecx
@L6:
mov esi,edi
sal esi,byte +24
mov dword [16+esp],esi
mov ebx,dword [24+esp]
mov esi,dword [4252+esp]
mov dl,byte [1+ebp]
mov al,byte [esi+ebx]
and edx,dword +255
and eax,dword +255
add eax,edx
mov dl,byte [2+ebp]
and edx,dword +255
add eax,edx
mov ecx,edi
mov esi,dword [16+esp]
sal eax,byte +4
sal ecx,byte +4
mov ebx,dword [116+esp]
or eax,esi
mov dword [4108+ebx+ecx],eax
lea ecx,dword [1+edi]
mov ebx,ecx
lea esi,dword [3+ebp]
sal ebx,byte +4
mov dword [16+esp],esi
mov dword [20+esp],ebx
mov esi,dword [24+esp]
mov ebx,dword [4252+esp]
mov al,byte [3+ebx+esi]
mov esi,dword [16+esp]
mov dl,byte [1+esi]
and eax,dword +255
and edx,dword +255
add eax,edx
mov dl,byte [2+esi]
and edx,dword +255
add eax,edx
sal ecx,byte +24
sal eax,byte +4
mov ebx,dword [20+esp]
mov esi,dword [116+esp]
or ecx,eax
mov dword [4108+esi+ebx],ecx
lea ecx,dword [2+edi]
mov esi,ecx
lea ebx,dword [6+ebp]
sal esi,byte +4
mov dword [16+esp],ebx
mov dword [20+esp],esi
mov ebx,dword [24+esp]
mov esi,dword [4252+esp]
mov al,byte [6+esi+ebx]
mov ebx,dword [16+esp]
mov dl,byte [1+ebx]
and eax,dword +255
and edx,dword +255
add eax,edx
mov dl,byte [2+ebx]
and edx,dword +255
add eax,edx
sal ecx,byte +24
sal eax,byte +4
mov esi,dword [20+esp]
mov ebx,dword [116+esp]
or ecx,eax
mov dword [4108+ebx+esi],ecx
lea ecx,dword [3+edi]
mov ebx,ecx
lea esi,dword [9+ebp]
sal ebx,byte +4
mov dword [16+esp],esi
mov dword [20+esp],ebx
mov esi,dword [24+esp]
mov ebx,dword [4252+esp]
mov al,byte [9+ebx+esi]
mov esi,dword [16+esp]
mov dl,byte [1+esi]
and eax,dword +255
and edx,dword +255
add eax,edx
mov dl,byte [2+esi]
and edx,dword +255
add eax,edx
sal ecx,byte +24
sal eax,byte +4
mov ebx,dword [20+esp]
mov esi,dword [116+esp]
or ecx,eax
mov dword [4108+esi+ebx],ecx
mov eax,dword [24+esp]
add eax,dword +12
add ebp,dword +12
mov dword [24+esp],eax
add edi,dword +4
jne @L6
push dword +255
push dword +0
lea eax,dword [164+esp]
push eax
call @_quicksort
add esp,dword +12
mov edi,dword +-256
lea ebp,dword [144+esp]
@L11:
mov ecx,edi
sal ecx,byte +4
mov al,byte [4111+ecx+ebp]
add ecx,dword +4096
and eax,dword +255
mov ebx,dword [4252+esp]
lea eax,dword [eax+eax*2]
mov dl,byte [ebx+eax]
and edx,dword +255
sal edx,byte +4
add eax,ebx
mov dword [ecx+ebp],edx
mov dl,byte [1+eax]
and edx,dword +255
sal edx,byte +4
mov dword [4+ecx+ebp],edx
mov dl,byte [2+eax]
xor eax,eax
mov al,dl
sal eax,byte +4
mov dword [8+ecx+ebp],eax
mov ecx,edi
sal ecx,byte +4
mov al,byte [4127+ecx+ebp]
add ecx,dword +4112
and eax,dword +255
lea eax,dword [eax+eax*2]
mov dl,byte [ebx+eax]
and edx,dword +255
sal edx,byte +4
add eax,ebx
mov dword [ecx+ebp],edx
mov dl,byte [1+eax]
and edx,dword +255
sal edx,byte +4
mov dword [4+ecx+ebp],edx
mov al,byte [2+eax]
and eax,dword +255
sal eax,byte +4
mov dword [8+ecx+ebp],eax
mov ecx,edi
sal ecx,byte +4
mov al,byte [4143+ecx+ebp]
add ecx,dword +4128
and eax,dword +255
lea eax,dword [eax+eax*2]
mov dl,byte [ebx+eax]
and edx,dword +255
sal edx,byte +4
add eax,ebx
mov dword [ecx+ebp],edx
mov dl,byte [1+eax]
and edx,dword +255
sal edx,byte +4
mov dword [4+ecx+ebp],edx
mov dl,byte [2+eax]
xor eax,eax
mov al,dl
sal eax,byte +4
mov dword [8+ecx+ebp],eax
mov ecx,edi
sal ecx,byte +4
mov al,byte [4159+ecx+ebp]
add ecx,dword +4144
and eax,dword +255
lea eax,dword [eax+eax*2]
mov dl,byte [ebx+eax]
and edx,dword +255
sal edx,byte +4
add eax,ebx
mov dword [ecx+ebp],edx
mov dl,byte [1+eax]
and edx,dword +255
sal edx,byte +4
mov dword [4+ecx+ebp],edx
mov bl,byte [2+eax]
xor eax,eax
mov al,bl
sal eax,byte +4
mov dword [8+ecx+ebp],eax
add edi,dword +4
jne @L11
@L2:
mov esi,dword [4272+esp]
mov eax,dword [4260+esp]
push esi
push eax
call @_ResizingTable
mov dword [120+esp],eax
mov ebx,dword [4284+esp]
mov esi,dword [4272+esp]
push ebx
push esi
call @_ResizingTable
mov ebx,dword [4272+esp]
mov dword [124+esp],eax
mov dword [100+esp],ebx
mov eax,dword [4284+esp]
add esp,dword +16
test eax,eax
jge @L13
mov eax,dword [4264+esp]
mov edx,dword [4268+esp]
dec eax
mov ecx,dword [4248+esp]
{imul eax,edx} db $0F,$AF,$C2
sub ecx,eax
mov dword [4248+esp],ecx
@L13:
mov eax,dword [4280+esp]
test eax,eax
jge @L14
mov eax,dword [4276+esp]
mov edx,dword [4280+esp]
dec eax
mov ecx,dword [84+esp]
{imul eax,edx} db $0F,$AF,$C2
sub ecx,eax
mov dword [84+esp],ecx
@L14:
xor eax,eax
mov dword [100+esp],eax
mov esi,dword [4276+esp]
mov edx,dword [100+esp]
cmp edx,esi
jge @L16
lea ebx,dword [144+esp]
mov dword [56+esp],ebx
@L18:
mov esi,dword [4248+esp]
mov dword [92+esp],esi
mov ebx,dword [100+esp]
mov esi,dword [108+esp]
mov ebx,dword [esi+ebx*4]
mov dword [96+esp],ebx
test bl,byte +127
je @L19
mov eax,ebx
sar eax,byte +8
and eax,dword +255
mov dword [76+esp],eax
jmp @L20
@L19:
mov ecx,dword +256
mov dword [76+esp],ecx
@L20:
xor eax,eax
mov dword [104+esp],eax
mov ebx,dword [4272+esp]
mov esi,dword [104+esp]
cmp esi,ebx
jge @L22
mov esi,dword [96+esp]
and esi,dword +127
mov dword [36+esp],esi
mov ebx,dword [84+esp]
mov esi,dword [104+esp]
mov dword [32+esp],ebx
mov dword [28+esp],esi
@L24:
mov ebx,dword [76+esp]
mov dword [80+esp],ebx
mov esi,dword [104+esp]
mov ebx,dword [112+esp]
mov ebp,dword [ebx+esi*4]
test ebp,dword +127
je @L25
mov eax,ebp
sar eax,byte +8
and eax,dword +255
mov dword [72+esp],eax
jmp @L26
@L25:
mov edx,dword +256
mov dword [72+esp],edx
@L26:
mov esi,dword [92+esp]
xor ecx,ecx
mov dword [88+esp],esi
mov dword [140+esp],ecx
mov dword [136+esp],ecx
mov dword [132+esp],ecx
mov edi,dword [4244+esp]
test edi,edi
jne @L27
mov eax,dword [36+esp]
cmp edi,eax
jg @L29
mov ebx,ebp
shr ebx,byte +24
mov dword [60+esp],ebx
@L31:
xor edx,edx
mov ecx,dword [72+esp]
xor eax,eax
mov esi,ebp
mov dword [128+esp],edx
mov dword [124+esp],edx
mov dword [120+esp],edx
and esi,dword +127
mov dword [16+esp],eax
mov edx,dword [88+esp]
mov dword [20+esp],esi
cmp eax,esi
jg @L33
@L35:
mov al,byte [edx]
and eax,dword +255
mov ebx,dword [128+esp]
{imul eax,ecx} db $0F,$AF,$C1
add ebx,eax
inc edx
mov dword [128+esp],ebx
mov al,byte [edx]
and eax,dword +255
mov esi,dword [124+esp]
{imul eax,ecx} db $0F,$AF,$C1
add esi,eax
inc edx
mov dword [124+esp],esi
mov al,byte [edx]
and eax,dword +255
{imul eax,ecx} db $0F,$AF,$C1
mov ecx,dword [120+esp]
add ecx,eax
mov dword [120+esp],ecx
mov eax,dword [16+esp]
inc eax
mov ebx,dword [20+esp]
inc edx
mov dword [16+esp],eax
cmp eax,ebx
jne @L36
mov ecx,dword [60+esp]
jmp @L113
@L36:
mov ecx,ebp
sar ecx,byte +16
@L113:
and ecx,dword +255
mov esi,ebp
and esi,dword +127
mov eax,dword [16+esp]
mov dword [20+esp],esi
cmp eax,esi
jle @L35
@L33:
mov ebx,dword [4268+esp]
mov edx,dword [88+esp]
add edx,ebx
mov dword [88+esp],edx
mov eax,dword [80+esp]
mov ecx,dword [120+esp]
mov edx,dword [132+esp]
{imul eax,ecx} db $0F,$AF,$C1
add edx,eax
mov dword [132+esp],edx
mov ecx,dword [124+esp]
mov eax,dword [80+esp]
mov edx,dword [136+esp]
{imul eax,ecx} db $0F,$AF,$C1
add edx,eax
mov dword [136+esp],edx
mov ecx,dword [128+esp]
mov eax,dword [80+esp]
mov edx,dword [140+esp]
{imul eax,ecx} db $0F,$AF,$C1
add edx,eax
inc edi
mov ecx,dword [36+esp]
mov dword [140+esp],edx
cmp edi,ecx
jne @L39
mov esi,dword [96+esp]
shr esi,byte +24
mov dword [80+esp],esi
jmp @L28
@L39:
mov eax,dword [96+esp]
sar eax,byte +16
and eax,dword +255
mov dword [80+esp],eax
@L28:
mov eax,dword [36+esp]
cmp edi,eax
jle @L31
@L29:
mov eax,ebp
and eax,dword +127
mov edx,dword [92+esp]
lea eax,dword [eax+eax*2]
add edx,eax
mov dword [92+esp],edx
mov ebx,ebp
test bl,bl
jge @L43
add edx,dword +3
jmp @L114
@L27:
xor edi,edi
mov ecx,dword [36+esp]
cmp edi,ecx
jg @L45
@L47:
xor eax,eax
mov ecx,dword [72+esp]
mov edx,ebp
mov dword [128+esp],eax
mov dword [124+esp],eax
mov dword [120+esp],eax
mov dword [16+esp],eax
and edx,dword +127
cmp eax,edx
jg @L49
@L51:
mov esi,dword [88+esp]
mov ebx,dword [16+esp]
mov al,byte [ebx+esi]
and eax,dword +255
mov esi,dword [4244+esp]
lea eax,dword [eax+eax*2]
mov al,byte [esi+eax]
and eax,dword +255
mov esi,dword [120+esp]
{imul eax,ecx} db $0F,$AF,$C1
add esi,eax
mov dword [120+esp],esi
mov esi,dword [88+esp]
mov al,byte [ebx+esi]
and eax,dword +255
mov ebx,dword [4244+esp]
lea eax,dword [eax+eax*2]
mov al,byte [1+ebx+eax]
and eax,dword +255
mov ebx,dword [124+esp]
{imul eax,ecx} db $0F,$AF,$C1
add ebx,eax
mov dword [124+esp],ebx
mov ebx,dword [16+esp]
mov al,byte [ebx+esi]
and eax,dword +255
mov esi,dword [4244+esp]
lea eax,dword [eax+eax*2]
mov al,byte [2+esi+eax]
and eax,dword +255
mov esi,dword [128+esp]
inc ebx
{imul eax,ecx} db $0F,$AF,$C1
add esi,eax
mov dword [16+esp],ebx
mov dword [128+esp],esi
cmp ebx,edx
jne @L52
mov ecx,ebp
shr ecx,byte +24
jmp @L48
@L52:
mov ecx,ebp
sar ecx,byte +16
and ecx,dword +255
@L48:
mov edx,ebp
mov eax,dword [16+esp]
and edx,dword +127
cmp eax,edx
jle @L51
@L49:
mov ebx,dword [4268+esp]
mov edx,dword [88+esp]
add edx,ebx
mov dword [88+esp],edx
mov eax,dword [80+esp]
mov ecx,dword [120+esp]
mov edx,dword [132+esp]
{imul eax,ecx} db $0F,$AF,$C1
add edx,eax
mov dword [132+esp],edx
mov ecx,dword [124+esp]
mov eax,dword [80+esp]
mov edx,dword [136+esp]
{imul eax,ecx} db $0F,$AF,$C1
add edx,eax
mov dword [136+esp],edx
mov ecx,dword [128+esp]
mov eax,dword [80+esp]
mov edx,dword [140+esp]
{imul eax,ecx} db $0F,$AF,$C1
add edx,eax
inc edi
mov ecx,dword [36+esp]
mov dword [140+esp],edx
cmp edi,ecx
jne @L55
mov esi,dword [96+esp]
shr esi,byte +24
mov dword [80+esp],esi
jmp @L44
@L55:
mov eax,dword [96+esp]
sar eax,byte +16
and eax,dword +255
mov dword [80+esp],eax
@L44:
mov eax,dword [36+esp]
cmp edi,eax
jle @L47
@L45:
mov eax,ebp
mov edx,dword [92+esp]
and eax,dword +127
add edx,eax
mov dword [92+esp],edx
mov ebx,ebp
test bl,bl
jge @L43
inc edx
@L114:
mov dword [92+esp],edx
@L43:
mov ecx,dword [4252+esp]
test ecx,ecx
jne @L59
movsx eax,word [142+esp]
mov esi,dword [28+esp]
mov ebx,dword [84+esp]
mov byte [ebx+esi],al
movsx eax,word [138+esp]
mov esi,dword [32+esp]
mov byte [1+esi],al
movsx eax,word [134+esp]
mov byte [2+esi],al
jmp @L23
@L59:
mov ecx,dword [132+esp]
mov edx,dword [136+esp]
sar ecx,byte +12
mov eax,dword [140+esp]
sar edx,byte +12
mov dword [132+esp],ecx
sar eax,byte +12
add ecx,edx
mov dword [140+esp],eax
mov edi,dword +128
add ecx,eax
xor eax,eax
mov dword [136+esp],edx
mov dword [64+esp],ecx
mov dword [16+esp],eax
@L64:
mov edx,dword [16+esp]
add edx,edi
mov eax,edx
mov ebx,dword [56+esp]
sal eax,byte +4
mov eax,dword [12+eax+ebx]
mov ecx,dword [64+esp]
and eax,dword +16777215
cmp eax,ecx
jge @L63
mov dword [16+esp],edx
@L63:
sar edi,dword +1
jne @L64
mov ecx,dword [16+esp]
mov esi,dword [56+esp]
sal ecx,byte +4
mov ebx,dword [12+ecx+esi]
and ebx,dword +16777215
mov dword [64+esp],ebx
mov ebx,dword [56+esp]
mov esi,dword [132+esp]
mov edx,dword [ecx+ebx]
mov dword [52+esp],esi
sub edx,esi
jns @L67
neg edx
@L67:
mov esi,dword [56+esp]
mov ebp,dword [136+esp]
mov eax,dword [4+ecx+esi]
sub eax,ebp
jns @L68
neg eax
@L68:
mov ebx,dword [56+esp]
lea edi,dword [eax+edx]
mov edx,dword [140+esp]
mov eax,dword [8+ecx+ebx]
sub eax,edx
jns @L69
neg eax
@L69:
add edi,eax
mov ecx,dword [16+esp]
mov dword [68+esp],edi
mov edi,ecx
dec edi
js @L71
mov esi,dword [52+esp]
mov dword [44+esp],ebp
mov dword [40+esp],edx
mov dword [48+esp],esi
@L90:
inc ecx
cmp ecx,dword +255
jg @L71
test edi,edi
jl @L74
mov ebx,edi
mov esi,dword [56+esp]
sal ebx,byte +4
mov ebp,dword [12+ebx+esi]
and ebp,dword +16777215
mov edx,dword [64+esp]
mov eax,ebp
mov dword [20+esp],ebx
sub eax,edx
jns @L76
neg eax
@L76:
mov edx,dword [68+esp]
cmp edx,eax
jg @L75
xor edi,edi
jmp @L74
@L75:
mov ebx,dword [56+esp]
mov esi,dword [20+esp]
mov eax,dword [48+esp]
mov edx,dword [esi+ebx]
sub edx,eax
jns @L78
neg edx
@L78:
mov ebx,dword [56+esp]
mov esi,dword [20+esp]
mov eax,dword [4+esi+ebx]
mov ebx,dword [44+esp]
sub eax,ebx
jns @L79
neg eax
@L79:
mov ebx,dword [56+esp]
mov esi,dword [20+esp]
add edx,eax
mov eax,dword [8+esi+ebx]
mov esi,dword [40+esp]
sub eax,esi
jns @L80
neg eax
@L80:
add eax,edx
mov edx,dword [68+esp]
cmp eax,edx
jge @L74
mov dword [68+esp],eax
mov dword [16+esp],edi
mov dword [64+esp],ebp
@L74:
cmp ecx,dword +255
jg @L70
mov ebx,ecx
mov esi,dword [56+esp]
sal ebx,byte +4
mov ebp,dword [12+ebx+esi]
and ebp,dword +16777215
mov edx,dword [64+esp]
mov eax,ebp
mov dword [20+esp],ebx
sub eax,edx
jns @L84
neg eax
@L84:
mov edx,dword [68+esp]
cmp edx,eax
jg @L83
mov ecx,dword +255
jmp @L70
@L83:
mov ebx,dword [56+esp]
mov esi,dword [20+esp]
mov eax,dword [48+esp]
mov edx,dword [esi+ebx]
sub edx,eax
jns @L86
neg edx
@L86:
mov ebx,dword [56+esp]
mov esi,dword [20+esp]
mov eax,dword [4+esi+ebx]
mov ebx,dword [44+esp]
sub eax,ebx
jns @L87
neg eax
@L87:
mov ebx,dword [56+esp]
mov esi,dword [20+esp]
add edx,eax
mov eax,dword [8+esi+ebx]
mov esi,dword [40+esp]
sub eax,esi
jns @L88
neg eax
@L88:
add eax,edx
mov edx,dword [68+esp]
cmp eax,edx
jge @L70
mov dword [68+esp],eax
mov dword [16+esp],ecx
mov dword [64+esp],ebp
@L70:
dec edi
jns @L90
@L71:
mov eax,dword [16+esp]
mov ebx,dword [56+esp]
sal eax,byte +4
mov esi,dword [84+esp]
mov cl,byte [15+eax+ebx]
xor eax,eax
mov ebx,dword [104+esp]
mov al,cl
mov byte [ebx+esi],al
@L23:
mov eax,dword [32+esp]
mov edx,dword [28+esp]
mov ecx,dword [104+esp]
mov esi,dword [4272+esp]
add eax,dword +3
add edx,dword +3
inc ecx
mov dword [32+esp],eax
mov dword [28+esp],edx
mov dword [104+esp],ecx
cmp ecx,esi
jl @L24
@L22:
mov ebx,dword [4280+esp]
mov edx,dword [84+esp]
add edx,ebx
mov eax,dword [96+esp]
mov dword [84+esp],edx
mov ecx,dword [4268+esp]
and eax,dword +127
mov edx,dword [4248+esp]
{imul eax,ecx} db $0F,$AF,$C1
add edx,eax
mov dword [4248+esp],edx
mov cl,byte [96+esp]
test cl,cl
jge @L17
mov esi,dword [4268+esp]
add edx,esi
mov dword [4248+esp],edx
@L17:
mov eax,dword [100+esp]
inc eax
mov ebx,dword [4276+esp]
mov dword [100+esp],eax
cmp eax,ebx
jl @L18
@L16:
mov esi,dword [108+esp]
push esi
call free
mov ebx,dword [116+esp]
push ebx
call free
add esp,dword +8
pop ebx
pop esi
pop edi
pop ebp
add esp,dword +4224
ret
   (* .globl _ResizingTable *)
@_ResizingTable:
sub esp,dword +44
push ebp
push edi
push esi
push ebx
fld dword [@_f256]
fidiv dword [64+esp]
mov ecx,dword [68+esp]
lea ecx,dword [0+ecx*4]
fstp tbyte [20+esp]
push ecx
call malloc
xor esi,esi
mov dword [48+esp],eax
mov dword [52+esp],esi
add esp,dword +4
mov ebp,esi
fld tbyte [20+esp]
mov edx,dword [68+esp]
cmp ebp,edx
jge @L128
mov eax,dword [64+esp]
fld dword [@_f05]
mov dword [32+esp],eax
@L119:
mov edi,dword [68+esp]
mov ecx,dword [48+esp]
mov eax,dword [32+esp]
mov dword [16+esp],esi
sub edi,ecx
mov edx,eax
sar edx,byte +31
idiv dword [68+esp]
mov esi,eax
mov dword [48+esp],edx
test edx,edx
je @L120
mov ebx,edx
mov ecx,esi
mov eax,dword [16+esp]
xor edx,edx
jmp @L127
@L120:
lea ecx,dword [-1+esi]
mov eax,dword [16+esp]
mov edx,dword +128
mov ebx,dword [68+esp]
@L127:
sub ecx,eax
mov dword [40+esp],edx
je @L122
fld st(1)
push edi
fimul dword [esp]
add esp,dword +4
fadd st,st(1)
fnstcw [56+esp]
mov eax,dword [56+esp]
mov ah,byte +12
mov dword [52+esp],eax
fldcw [52+esp]
fistp dword [36+esp]
fldcw [56+esp]
cmp ecx,dword +1
je @L123
mov eax,dword [64+esp]
sub eax,edi
sub eax,ebx
fld st(1)
push eax
fimul dword [esp]
add esp,dword +4
lea eax,dword [-1+ecx]
push eax
fidiv dword [esp]
add esp,dword +4
fadd st,st(1)
fnstcw [56+esp]
mov eax,dword [56+esp]
mov ah,byte +12
mov dword [52+esp],eax
fldcw [52+esp]
sub esp,dword +4
fistp dword [esp]
pop edi
fldcw [56+esp]
jmp @L124
@L123:
xor edi,edi
@L124:
fld st(1)
push ebx
fimul dword [esp]
add esp,dword +4
fadd st,st(1)
fnstcw [56+esp]
mov eax,dword [56+esp]
mov ah,byte +12
mov dword [52+esp],eax
fldcw [52+esp]
sub esp,dword +4
fistp dword [esp]
pop ebx
fldcw [56+esp]
cmp edi,dword +255
jle @L125
mov edi,dword +255
@L125:
mov eax,dword [36+esp]
mov edx,edi
sal eax,byte +8
sal edx,byte +16
or eax,edx
mov edx,ebx
sal edx,byte +24
or eax,edx
or ecx,eax
@L122:
mov eax,dword [40+esp]
mov edx,dword [44+esp]
or ecx,eax
mov dword [edx+ebp*4],ecx
mov eax,dword [32+esp]
mov edx,dword [64+esp]
add eax,edx
mov dword [32+esp],eax
inc ebp
mov edx,dword [68+esp]
cmp ebp,edx
jl @L119
fstp st(0)
@L128:
fstp st(0)
mov eax,dword [44+esp]
pop ebx
pop esi
pop edi
pop ebp
add esp,dword +44
ret
   (* .globl _quicksort *)
@_quicksort:
push eax
push eax
push ebp
push edi
mov edi,dword [24+esp]
push esi
push ebx
mov ebx,dword [36+esp]
lea eax,dword [ebx+edi]
mov ecx,eax
shr ecx,byte +31
add eax,ecx
sar eax,dword +1
mov esi,dword [28+esp]
sal eax,byte +4
mov dword [16+esp],ebx
mov ecx,edi
mov ebp,dword [esi+eax]
sal ebx,byte +4
sal ecx,byte +4
and ebp,dword +16777215
@L146:
mov edx,dword [28+esp]
mov eax,dword [edx+ecx]
and eax,dword +16777215
cmp eax,ebp
jge @L150
@L151:
add ecx,dword +16
mov esi,dword [28+esp]
mov eax,dword [esi+ecx]
inc edi
and eax,dword +16777215
cmp eax,ebp
jl @L151
@L150:
mov edx,dword [28+esp]
mov eax,dword [edx+ebx]
and eax,dword +16777215
cmp eax,ebp
jle @L161
@L155:
mov eax,dword [16+esp]
dec eax
add ebx,dword +-16
mov esi,dword [28+esp]
mov dword [16+esp],eax
mov eax,dword [esi+ebx]
and eax,dword +16777215
cmp eax,ebp
jg @L155
@L161:
mov edx,dword [16+esp]
cmp edi,edx
jg @L162
mov esi,dword [28+esp]
mov edx,dword [28+esp]
mov eax,dword [esi+ebx]
mov edx,dword [edx+ecx]
mov dword [esi+ecx],eax
mov eax,dword [16+esp]
inc edi
mov dword [esi+ebx],edx
dec eax
add ecx,dword +16
add ebx,dword +-16
mov dword [16+esp],eax
cmp edi,eax
jle @L146
@L162:
mov edx,dword [32+esp]
mov esi,dword [16+esp]
cmp esi,edx
jle @L159
push esi
mov esi,dword [32+esp]
push edx
push esi
call @_quicksort
add esp,dword +12
@L159:
mov edx,dword [36+esp]
cmp edi,edx
jge @L160
mov esi,dword [28+esp]
push edx
push edi
push esi
call @_quicksort
add esp,dword +12
@L160:
pop ebx
pop esi
pop edi
pop ebp
add esp,dword +8
ret
   (* .globl _f256 *)
   (* .data *)
   (* .p2align 2 *)
@_f256:
dd  $43800000
   (* .globl _f05 *)
   (* .p2align 2 *)
@_f05:
dd  $3f000000
