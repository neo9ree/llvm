; RUN: llc < %s -mtriple=aarch64-unknown-unknown | FileCheck %s

; Compare if negative and select of constants where one constant is zero.

define i32 @neg_sel_constants(i32 %a) {
; CHECK-LABEL: neg_sel_constants:
; CHECK:       // BB#0:
; CHECK-NEXT:    mov w8, #5
; CHECK-NEXT:    and w0, w8, w0, asr #31
; CHECK-NEXT:    ret
;
  %tmp.1 = icmp slt i32 %a, 0
  %retval = select i1 %tmp.1, i32 5, i32 0
  ret i32 %retval
}

; Compare if negative and select of constants where one constant is zero and the other is a single bit.

define i32 @neg_sel_special_constant(i32 %a) {
; CHECK-LABEL: neg_sel_special_constant:
; CHECK:       // BB#0:
; CHECK-NEXT:    lsr w8, w0, #22
; CHECK-NEXT:    and w0, w8, #0x200
; CHECK-NEXT:    ret
;
  %tmp.1 = icmp slt i32 %a, 0
  %retval = select i1 %tmp.1, i32 512, i32 0
  ret i32 %retval
}

; Compare if negative and select variable or zero.

define i32 @neg_sel_variable_and_zero(i32 %a, i32 %b) {
; CHECK-LABEL: neg_sel_variable_and_zero:
; CHECK:       // BB#0:
; CHECK-NEXT:    and w0, w1, w0, asr #31
; CHECK-NEXT:    ret
;
  %tmp.1 = icmp slt i32 %a, 0
  %retval = select i1 %tmp.1, i32 %b, i32 0
  ret i32 %retval
}

; Compare if not positive and select the same variable as being compared: smin(a, 0).

define i32 @not_pos_sel_same_variable(i32 %a) {
; CHECK-LABEL: not_pos_sel_same_variable:
; CHECK:       // BB#0:
; CHECK-NEXT:    and w0, w0, w0, asr #31
; CHECK-NEXT:    ret
;
  %tmp = icmp slt i32 %a, 1
  %min = select i1 %tmp, i32 %a, i32 0
  ret i32 %min
}

; Flipping the comparison condition can be handled by getting the bitwise not of the sign mask.

; Compare if positive and select of constants where one constant is zero.

define i32 @pos_sel_constants(i32 %a) {
; CHECK-LABEL: pos_sel_constants:
; CHECK:       // BB#0:
; CHECK-NEXT:    mov w8, #5
; CHECK-NEXT:    bic w0, w8, w0, asr #31
; CHECK-NEXT:    ret
;
  %tmp.1 = icmp sgt i32 %a, -1
  %retval = select i1 %tmp.1, i32 5, i32 0
  ret i32 %retval
}

; Compare if positive and select of constants where one constant is zero and the other is a single bit.

define i32 @pos_sel_special_constant(i32 %a) {
; CHECK-LABEL: pos_sel_special_constant:
; CHECK:       // BB#0:
; CHECK-NEXT:    orr w8, wzr, #0x200
; CHECK-NEXT:    bic w0, w8, w0, lsr #22
; CHECK-NEXT:    ret
;
  %tmp.1 = icmp sgt i32 %a, -1
  %retval = select i1 %tmp.1, i32 512, i32 0
  ret i32 %retval
}

; Compare if positive and select variable or zero.

define i32 @pos_sel_variable_and_zero(i32 %a, i32 %b) {
; CHECK-LABEL: pos_sel_variable_and_zero:
; CHECK:       // BB#0:
; CHECK-NEXT:    bic w0, w1, w0, asr #31
; CHECK-NEXT:    ret
;
  %tmp.1 = icmp sgt i32 %a, -1
  %retval = select i1 %tmp.1, i32 %b, i32 0
  ret i32 %retval
}

; Compare if not negative or zero and select the same variable as being compared: smax(a, 0).

define i32 @not_neg_sel_same_variable(i32 %a) {
; CHECK-LABEL: not_neg_sel_same_variable:
; CHECK:       // BB#0:
; CHECK-NEXT:    bic w0, w0, w0, asr #31
; CHECK-NEXT:    ret
;
  %tmp = icmp sgt i32 %a, 0
  %min = select i1 %tmp, i32 %a, i32 0
  ret i32 %min
}

; https://llvm.org/bugs/show_bug.cgi?id=31175

; ret = (x-y) > 0 ? x-y : 0
define i32 @PR31175(i32 %x, i32 %y) {
; CHECK-LABEL: PR31175:
; CHECK:       // BB#0:
; CHECK-NEXT:    sub w8, w0, w1
; CHECK-NEXT:    bic w0, w8, w8, asr #31
; CHECK-NEXT:    ret
;
  %sub = sub nsw i32 %x, %y
  %cmp = icmp sgt i32 %sub, 0
  %sel = select i1 %cmp, i32 %sub, i32 0
  ret i32 %sel
}

