(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) 1996-99 Armin Rigo

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

Contact the author Armin Rigo by e-mail: arigo@planetquake.com
or by mail: Armin Rigo, La Cure, 1854 Leysin, Switzerland.
See also http://www.planetquake.com/quark
**************************************************************************)

   (* THIS IS SUPPOSED TO BE COMPILED THROUGH CCODE.PAS *)

   (* .file	"resizer.c" *)
   (* / GNU C version cygnus-2.7.2-970404 (i386-cygwin32) compiled by GNU C version cygnus-2.7.2-970404. *)
   (* / options passed:  -O3 *)
   (* / options enabled:  -fdefer-pop -fcse-follow-jumps -fcse-skip-blocks *)
   (* / -fexpensive-optimizations -fthread-jumps -fstrength-reduce -fpeephole *)
   (* / -fforce-mem -ffunction-cse -finline-functions -finline *)
   (* / -fkeep-static-consts -fcaller-saves -fpcc-struct-return *)
   (* / -frerun-cse-after-loop -fschedule-insns2 -fcommon -fverbose-asm *)
   (* / -fgnu-linker -m80387 -mhard-float -mno-soft-float -mieee-fp *)
   (* / -mfp-ret-in-387 -mschedule-prologue -mstack-arg-probe -mcpu=i386 *)
   (* / -march=pentium *)

   (* gcc2_compiled.: *)
@___gnu_compiled_c:
   (* .text *)
   (* .align 4 *)
   (* .globl _Resample *)
@_Resample:
(*push ebp
mov ebp,esp
mov eax,dword +4228
call _alloca*)
                 sub esp,4228
push edi
push esi
push ebx
push dword [36+ebp]
push dword [24+ebp]
call @_ResizingTable
mov dword [-4124+ebp],eax
push dword [40+ebp]
push dword [28+ebp]
call @_ResizingTable
mov dword [-4128+ebp],eax
mov ebx,dword [20+ebp]
mov dword [-4156+ebp],ebx
xor edi,edi
add esp,dword +16
lea esi,dword [-4096+ebp]
mov dword [-4180+ebp],esi
mov ebx,dword [16+ebp]
mov dword [-4224+ebp],ebx
mov dword [-4228+ebp],edi
mov ecx,edi
   (* .align 2,0x90 *)
@L5:
mov esi,dword [16+ebp]
movzx eax,byte [2+esi+edi*4]
movzx edx,byte [1+esi+edi*4]
add eax,edx
mov ebx,dword [-4224+ebp]
movzx edx,byte [ebx]
add eax,edx
sal eax,dword +4
or eax,dword [-4228+ebp]
mov esi,dword [-4180+ebp]
mov dword [12+ecx+esi],eax
add ebx,dword +4
mov dword [-4224+ebp],ebx
add dword [-4228+ebp],dword +16777216
add ecx,dword +16
inc edi
cmp edi,dword +255
jle @L5
push dword +255
push dword +0
lea eax,dword [-4084+ebp]
push eax
call @_quicksort
xor edi,edi
add esp,dword +12
lea ecx,dword [-4096+ebp]
mov edx,edi
   (* .align 2,0x90 *)
@L10:
movzx ebx,byte [15+edx+ecx]
mov dword [-4228+ebp],ebx
mov esi,dword [16+ebp]
movzx eax,byte [2+esi+ebx*4]
sal eax,dword +4
mov dword [ecx+edx],eax
movzx eax,byte [1+esi+ebx*4]
sal eax,dword +4
mov dword [4+edx+ecx],eax
movzx eax,byte [esi+ebx*4]
sal eax,dword +4
mov dword [8+edx+ecx],eax
add edx,dword +16
inc edi
cmp edi,dword +255
jle @L10
cmp dword [32+ebp],dword +0
jge @L12
mov eax,dword [28+ebp]
dec eax
imul eax,dword [32+ebp]
sub dword [12+ebp],eax
@L12:
cmp dword [44+ebp],dword +0
jge @L13
mov eax,dword [40+ebp]
dec eax
imul eax,dword [44+ebp]
sub dword [-4156+ebp],eax
@L13:
mov dword [-4136+ebp],dword +0
mov ebx,dword [40+ebp]
cmp dword [-4136+ebp],ebx
jge @L15
lea esi,dword [-4096+ebp]
mov dword [-4188+ebp],esi
   (* .align 2,0x90 *)
@L17:
mov ebx,dword [12+ebp]
mov dword [-4148+ebp],ebx
mov ebx,dword [-4136+ebp]
mov esi,dword [-4128+ebp]
mov ebx,dword [esi+ebx*4]
mov dword [-4140+ebp],ebx
test bl,byte +127
je @L18
mov eax,ebx
sar eax,dword +8
and eax,dword +255
mov dword [-4164+ebp],eax
jmp @L19
   (* .align 2,0x90 *)
@L18:
mov dword [-4164+ebp],dword +256
@L19:
mov dword [-4132+ebp],dword +0
mov ebx,dword [36+ebp]
cmp dword [-4132+ebp],ebx
jge @L21
mov esi,dword [-4140+ebp]
and esi,dword +127
mov dword [-4220+ebp],esi
   (* .align 2,0x90 *)
@L23:
mov ebx,dword [-4164+ebp]
mov dword [-4160+ebp],ebx
mov ebx,dword [-4132+ebp]
mov esi,dword [-4124+ebp]
mov ebx,dword [esi+ebx*4]
mov dword [-4144+ebp],ebx
test bl,byte +127
je @L24
mov eax,ebx
sar eax,dword +8
and eax,dword +255
mov dword [-4168+ebp],eax
jmp @L25
   (* .align 2,0x90 *)
@L24:
mov dword [-4168+ebp],dword +256
@L25:
mov ebx,dword [-4148+ebp]
mov dword [-4152+ebp],ebx
mov dword [-4100+ebp],dword +0
mov dword [-4104+ebp],dword +0
mov dword [-4108+ebp],dword +0
cmp dword [8+ebp],dword +0
jne @L26
mov edi,dword [8+ebp]
cmp dword [-4220+ebp],edi
jl @L28
mov esi,dword [-4144+ebp]
shr esi,dword +24
mov dword [-4184+ebp],esi
   (* .align 2,0x90 *)
@L30:
mov dword [-4112+ebp],dword +0
mov dword [-4116+ebp],dword +0
mov dword [-4120+ebp],dword +0
mov ecx,dword [-4168+ebp]
mov edx,dword [-4152+ebp]
mov dword [-4228+ebp],dword +0
jmp @L31
   (* .align 2,0x90 *)
   (* .align 2,0x90 *)
@L34:
movzx eax,byte [edx]
{$IFDEF IMUL_BUG}
imul ecx,eax
{$ELSE}
imul eax,ecx
{$ENDIF}
add dword [-4112+ebp],eax
inc edx
movzx eax,byte [edx]
{$IFDEF IMUL_BUG}
imul ecx,eax
{$ELSE}
imul eax,ecx
{$ENDIF}
add dword [-4116+ebp],eax
inc edx
movzx eax,byte [edx]
{$IFDEF IMUL_BUG}
imul ecx,eax
{$ELSE}
imul eax,ecx
{$ENDIF}
add dword [-4120+ebp],eax
inc edx
inc dword [-4228+ebp]
mov esi,dword [-4224+ebp]
cmp dword [-4228+ebp],esi
jne @L35
mov ecx,dword [-4184+ebp]
jmp @L91
   (* .align 2,0x90 *)
@L35:
mov eax,dword [-4144+ebp]
sar eax,dword +16
mov ecx,eax
@L91:
movzx ecx,cl
@L31:
mov ebx,dword [-4144+ebp]
and ebx,dword +127
mov dword [-4224+ebp],ebx
cmp dword [-4228+ebp],ebx
jle @L34
mov esi,dword [32+ebp]
add dword [-4152+ebp],esi
mov eax,dword [-4160+ebp]
imul eax,dword [-4120+ebp]
add dword [-4108+ebp],eax
mov eax,dword [-4160+ebp]
imul eax,dword [-4116+ebp]
add dword [-4104+ebp],eax
mov eax,dword [-4160+ebp]
imul eax,dword [-4112+ebp]
add dword [-4100+ebp],eax
inc edi
cmp dword [-4220+ebp],edi
jne @L38
mov ebx,dword [-4140+ebp]
shr ebx,dword +24
mov dword [-4160+ebp],ebx
jmp @L27
   (* .align 2,0x90 *)
@L38:
mov eax,dword [-4140+ebp]
sar eax,dword +16
and eax,dword +255
mov dword [-4160+ebp],eax
@L27:
cmp dword [-4220+ebp],edi
jge @L30
@L28:
mov eax,dword [-4144+ebp]
and eax,dword +127
lea eax,dword [eax+eax*2]
add dword [-4148+ebp],eax
cmp byte [-4144+ebp],byte +0
jge @L42
add dword [-4148+ebp],dword +3
jmp @L42
   (* .align 2,0x90 *)
@L26:
xor edi,edi
@L92:
cmp dword [-4220+ebp],edi
jl @L44
mov dword [-4112+ebp],dword +0
mov dword [-4116+ebp],dword +0
mov dword [-4120+ebp],dword +0
mov ecx,dword [-4168+ebp]
mov dword [-4228+ebp],dword +0
jmp @L47
   (* .align 2,0x90 *)
   (* .align 2,0x90 *)
@L50:
mov ebx,dword [-4152+ebp]
mov esi,dword [-4228+ebp]
movzx eax,byte [esi+ebx]
mov ebx,dword [8+ebp]
mov edx,dword [ebx+eax*4]
mov eax,edx
shr eax,dword +16
and eax,dword +255
{$IFDEF IMUL_BUG}
imul ecx,eax
{$ELSE}
imul eax,ecx
{$ENDIF}
add dword [-4120+ebp],eax
mov eax,edx
shr eax,dword +8
and eax,dword +255
{$IFDEF IMUL_BUG}
imul ecx,eax
{$ELSE}
imul eax,ecx
{$ENDIF}
add dword [-4116+ebp],eax
movzx edx,dl
{$IFDEF IMUL_BUG}
imul ecx,edx
{$ELSE}
imul edx,ecx
{$ENDIF}
add dword [-4112+ebp],edx
inc esi
mov dword [-4228+ebp],esi
mov ebx,dword [-4224+ebp]
cmp esi,ebx
jne @L51
mov ecx,dword [-4144+ebp]
shr ecx,dword +24
jmp @L47
   (* .align 2,0x90 *)
@L51:
mov eax,dword [-4144+ebp]
sar eax,dword +16
mov ecx,eax
movzx ecx,cl
@L47:
mov esi,dword [-4144+ebp]
and esi,dword +127
mov dword [-4224+ebp],esi
cmp dword [-4228+ebp],esi
jle @L50
mov ebx,dword [32+ebp]
add dword [-4152+ebp],ebx
mov eax,dword [-4160+ebp]
imul eax,dword [-4120+ebp]
add dword [-4108+ebp],eax
mov eax,dword [-4160+ebp]
imul eax,dword [-4116+ebp]
add dword [-4104+ebp],eax
mov eax,dword [-4160+ebp]
imul eax,dword [-4112+ebp]
add dword [-4100+ebp],eax
inc edi
cmp dword [-4220+ebp],edi
jne @L54
mov esi,dword [-4140+ebp]
shr esi,dword +24
mov dword [-4160+ebp],esi
jmp @L92
   (* .align 2,0x90 *)
@L54:
mov eax,dword [-4140+ebp]
sar eax,dword +16
and eax,dword +255
mov dword [-4160+ebp],eax
jmp @L92
   (* .align 2,0x90 *)
@L44:
mov eax,dword [-4144+ebp]
and eax,dword +127
add dword [-4148+ebp],eax
cmp byte [-4144+ebp],byte +0
jge @L42
inc dword [-4148+ebp]
@L42:
mov ecx,dword [-4108+ebp]
sar ecx,dword +12
mov dword [-4108+ebp],ecx
mov edx,dword [-4104+ebp]
sar edx,dword +12
mov dword [-4104+ebp],edx
mov eax,dword [-4100+ebp]
sar eax,dword +12
mov dword [-4100+ebp],eax
add ecx,edx
add ecx,eax
mov dword [-4176+ebp],ecx
mov dword [-4228+ebp],dword +0
mov edi,dword +128
   (* .align 2,0x90 *)
@L61:
mov edx,dword [-4228+ebp]
add edx,edi
mov eax,edx
sal eax,dword +4
mov ebx,dword [-4188+ebp]
mov eax,dword [12+eax+ebx]
and eax,dword +16777215
cmp dword [-4176+ebp],eax
jle @L60
mov dword [-4228+ebp],edx
@L60:
sar edi,dword +1
jne @L61
mov ecx,dword [-4228+ebp]
sal ecx,dword +4
mov esi,dword [-4188+ebp]
mov ebx,dword [12+ecx+esi]
and ebx,dword +16777215
mov dword [-4176+ebp],ebx
mov esi,dword [-4108+ebp]
mov dword [-4192+ebp],esi
mov ebx,dword [-4188+ebp]
mov edx,dword [ecx+ebx]
sub edx,esi
jns @L64
neg edx
@L64:
mov esi,dword [-4104+ebp]
mov dword [-4196+ebp],esi
mov ebx,dword [-4188+ebp]
mov eax,dword [4+ecx+ebx]
sub eax,esi
jns @L65
neg eax
@L65:
lea edi,dword [eax+edx]
mov edx,dword [-4100+ebp]
mov esi,dword [-4188+ebp]
mov eax,dword [8+ecx+esi]
sub eax,edx
jns @L66
neg eax
@L66:
add edi,eax
mov dword [-4172+ebp],edi
mov ebx,dword [-4228+ebp]
mov dword [-4224+ebp],ebx
mov edi,ebx
dec edi
js @L68
mov esi,dword [-4192+ebp]
mov dword [-4204+ebp],esi
mov ebx,dword [-4196+ebp]
mov dword [-4208+ebp],ebx
mov dword [-4212+ebp],edx
   (* .align 2,0x90 *)
@L87:
inc dword [-4224+ebp]
cmp dword [-4224+ebp],dword +255
jg @L68
test edi,edi
jl @L71
mov ecx,edi
sal ecx,dword +4
mov esi,dword [-4188+ebp]
mov ebx,dword [12+ecx+esi]
and ebx,dword +16777215
mov dword [-4200+ebp],ebx
mov eax,ebx
sub eax,dword [-4176+ebp]
jns @L73
neg eax
@L73:
cmp dword [-4172+ebp],eax
jg @L72
xor edi,edi
jmp @L71
   (* .align 2,0x90 *)
@L72:
mov esi,dword [-4188+ebp]
mov edx,dword [ecx+esi]
sub edx,dword [-4204+ebp]
jns @L75
neg edx
@L75:
mov ebx,dword [-4188+ebp]
mov eax,dword [4+ecx+ebx]
sub eax,dword [-4208+ebp]
jns @L76
neg eax
@L76:
add edx,eax
mov esi,dword [-4188+ebp]
mov eax,dword [8+ecx+esi]
sub eax,dword [-4212+ebp]
jns @L77
neg eax
@L77:
add eax,edx
cmp dword [-4172+ebp],eax
jle @L71
mov dword [-4172+ebp],eax
mov dword [-4228+ebp],edi
mov ebx,dword [-4200+ebp]
mov dword [-4176+ebp],ebx
@L71:
cmp dword [-4224+ebp],dword +255
jg @L67
mov ecx,dword [-4224+ebp]
sal ecx,dword +4
mov esi,dword [-4188+ebp]
mov ebx,dword [12+ecx+esi]
and ebx,dword +16777215
mov dword [-4216+ebp],ebx
mov eax,ebx
sub eax,dword [-4176+ebp]
jns @L81
neg eax
@L81:
cmp dword [-4172+ebp],eax
jg @L80
mov dword [-4224+ebp],dword +255
jmp @L67
   (* .align 2,0x90 *)
@L80:
mov esi,dword [-4188+ebp]
mov edx,dword [ecx+esi]
sub edx,dword [-4204+ebp]
jns @L83
neg edx
@L83:
mov ebx,dword [-4188+ebp]
mov eax,dword [4+ecx+ebx]
sub eax,dword [-4208+ebp]
jns @L84
neg eax
@L84:
add edx,eax
mov esi,dword [-4188+ebp]
mov eax,dword [8+ecx+esi]
sub eax,dword [-4212+ebp]
jns @L85
neg eax
@L85:
add eax,edx
cmp dword [-4172+ebp],eax
jle @L67
mov dword [-4172+ebp],eax
mov ebx,dword [-4224+ebp]
mov dword [-4228+ebp],ebx
mov esi,dword [-4216+ebp]
mov dword [-4176+ebp],esi
@L67:
dec edi
jns @L87
@L68:
mov eax,dword [-4228+ebp]
sal eax,dword +4
mov ebx,dword [-4188+ebp]
movzx eax,byte [15+eax+ebx]
mov esi,dword [-4156+ebp]
mov ebx,dword [-4132+ebp]
mov byte [ebx+esi],al
inc ebx
mov dword [-4132+ebp],ebx
mov esi,dword [36+ebp]
cmp ebx,esi
jl @L23
@L21:
mov ebx,dword [44+ebp]
add dword [-4156+ebp],ebx
mov eax,dword [-4140+ebp]
and eax,dword +127
imul eax,dword [32+ebp]
add dword [12+ebp],eax
cmp byte [-4140+ebp],byte +0
jge @L16
mov esi,dword [32+ebp]
add dword [12+ebp],esi
@L16:
inc dword [-4136+ebp]
mov ebx,dword [40+ebp]
cmp dword [-4136+ebp],ebx
jl @L17
@L15:
push dword [-4128+ebp]
call free
push dword [-4124+ebp]
call free
lea esp,dword [-4240+ebp]
pop ebx
pop esi
pop edi
leave
ret
   (* .align 4 *)
   (* .globl _ResizingTable *)
@_ResizingTable:
push ebp
mov ebp,esp
sub esp,dword +56
push edi
push esi
push ebx
fld dword [@_f256]
fidiv dword [8+ebp]
mov eax,dword [12+ebp]
sal eax,dword +2
mov dword [-52+ebp],eax
push eax
fstp tbyte [-48+ebp]
call malloc
mov dword [-20+ebp],eax
xor esi,esi
mov dword [-12+ebp],esi
mov dword [-16+ebp],esi
fld tbyte [-48+ebp]
mov edx,dword [12+ebp]
cmp esi,edx
jge @L117
fld dword [@_f05]
mov dword [-32+ebp],eax
mov eax,dword [8+ebp]
mov dword [-36+ebp],eax
   (* .align 2,0x90 *)
@L109:
mov dword [-56+ebp],esi
mov edi,dword [12+ebp]
sub edi,dword [-12+ebp]
mov eax,dword [-36+ebp]
cdq
idiv dword [12+ebp]
mov esi,eax
mov dword [-12+ebp],edx
test edx,edx
je @L110
mov ebx,edx
mov ecx,esi
sub ecx,dword [-56+ebp]
mov dword [-24+ebp],dword +0
jmp @L111
   (* .align 2,0x90 *)
@L110:
lea ecx,dword [-1+esi]
mov ebx,dword [12+ebp]
sub ecx,dword [-56+ebp]
mov dword [-24+ebp],dword +128
@L111:
test ecx,ecx
je @L112
fld st(1)
push edi
fimul dword [esp]
add esp,dword +4
fadd st,st(1)
fnstcw [-4+ebp]
mov eax,dword [-4+ebp]
mov ah,byte +12
mov dword [-8+ebp],eax
fldcw [-8+ebp]
fistp dword [-28+ebp]
fldcw [-4+ebp]
cmp ecx,dword +1
je @L113
mov eax,dword [8+ebp]
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
fnstcw [-4+ebp]
mov eax,dword [-4+ebp]
mov ah,byte +12
mov dword [-8+ebp],eax
fldcw [-8+ebp]
sub esp,dword +4
fistp dword [esp]
pop edi
fldcw [-4+ebp]
jmp @L114
   (* .align 2,0x90 *)
@L113:
xor edi,edi
@L114:
fld st(1)
push ebx
fimul dword [esp]
add esp,dword +4
fadd st,st(1)
fnstcw [-4+ebp]
mov eax,dword [-4+ebp]
mov ah,byte +12
mov dword [-8+ebp],eax
fldcw [-8+ebp]
sub esp,dword +4
fistp dword [esp]
pop ebx
fldcw [-4+ebp]
cmp edi,dword +255
jle @L115
mov edi,dword +255
@L115:
mov eax,dword [-28+ebp]
sal eax,dword +8
mov edx,edi
sal edx,dword +16
or eax,edx
mov edx,ebx
sal edx,dword +24
or eax,edx
or ecx,eax
@L112:
or ecx,dword [-24+ebp]
mov eax,dword [-32+ebp]
mov dword [eax],ecx
add eax,dword +4
mov dword [-32+ebp],eax
mov edx,dword [8+ebp]
add dword [-36+ebp],edx
inc dword [-16+ebp]
mov eax,dword [12+ebp]
cmp dword [-16+ebp],eax
jl @L109
fstp st(0)
@L117:
fstp st(0)
mov eax,dword [-20+ebp]
lea esp,dword [-68+ebp]
pop ebx
pop esi
pop edi
leave
ret
   (* .align 4 *)
   (* .globl _quicksort *)
@_quicksort:
push ebp
mov ebp,esp
sub esp,dword +8
push edi
push esi
push ebx
mov edi,dword [12+ebp]
mov esi,dword [16+ebp]
lea eax,dword [esi+edi]
mov edx,eax
shr edx,dword +31
add eax,edx
sar eax,dword +1
sal eax,dword +4
mov ecx,dword [8+ebp]
mov eax,dword [ecx+eax]
and eax,dword +16777215
mov dword [-4+ebp],eax
mov eax,esi
sal eax,dword +4
lea ebx,dword [ecx+eax]
mov eax,edi
sal eax,dword +4
add eax,ecx
mov dword [-8+ebp],eax
   (* .align 2,0x90 *)
@L135:
mov ecx,dword [-8+ebp]
mov eax,dword [ecx]
and eax,dword +16777215
cmp dword [-4+ebp],eax
jle @L139
mov edx,edi
sal edx,dword +4
   (* .align 2,0x90 *)
@L140:
add edx,dword +16
add dword [-8+ebp],dword +16
inc edi
mov ecx,dword [8+ebp]
mov eax,dword [ecx+edx]
and eax,dword +16777215
cmp dword [-4+ebp],eax
jg @L140
@L139:
mov eax,dword [ebx]
and eax,dword +16777215
cmp dword [-4+ebp],eax
jge @L150
mov edx,esi
sal edx,dword +4
   (* .align 2,0x90 *)
@L144:
add edx,dword +-16
add ebx,dword +-16
dec esi
mov ecx,dword [8+ebp]
mov eax,dword [ecx+edx]
and eax,dword +16777215
cmp dword [-4+ebp],eax
jl @L144
@L150:
cmp edi,esi
jg @L151
mov ecx,dword [-8+ebp]
mov edx,dword [ecx]
mov eax,dword [ebx]
mov dword [ecx],eax
mov dword [ebx],edx
add ecx,dword +16
mov dword [-8+ebp],ecx
inc edi
add ebx,dword +-16
dec esi
cmp edi,esi
jle @L135
@L151:
cmp dword [12+ebp],esi
jge @L148
push esi
push dword [12+ebp]
push dword [8+ebp]
call @_quicksort
add esp,dword +12
@L148:
cmp dword [16+ebp],edi
jle @L149
push dword [16+ebp]
push edi
push dword [8+ebp]
call @_quicksort
@L149:
lea esp,dword [-20+ebp]
pop ebx
pop esi
pop edi
leave
ret
   (* .globl _f256 *)
   (* .data *)
   (* .align 4 *)
@_f256:
dd  $43800000
   (* .globl _f05 *)
   (* .align 4 *)
@_f05:
dd  $3f000000
