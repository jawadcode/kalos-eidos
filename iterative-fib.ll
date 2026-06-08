; ModuleID = 'KalosJIT'
source_filename = "KalosJIT"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"

define double @"operator:"(double %x, double %y) {
fun_entry:
  ret double %y
}

define double @fib(double %x) {
fun_entry:
  %x1 = alloca double, align 8
  store double %x, ptr %x1, align 8
  %less_than_temp = fcmp ult double %x, 3.000000e+00
  br i1 %less_than_temp, label %if_merge, label %if_else

if_else:                                          ; preds = %fun_entry
  %sub_temp = fadd double %x, -1.000000e+00
  %fun_call_temp = call double @fib(double %sub_temp)
  %sub_temp5 = fadd double %x, -2.000000e+00
  %fun_call_temp6 = call double @fib(double %sub_temp5)
  %add_temp = fadd double %fun_call_temp, %fun_call_temp6
  br label %if_merge

if_merge:                                         ; preds = %fun_entry, %if_else
  %if_temp = phi double [ %add_temp, %if_else ], [ 1.000000e+00, %fun_entry ]
  ret double %if_temp
}

define double @fibi(double %x) {
fun_entry:
  %i = alloca double, align 8
  %b = alloca double, align 8
  %a = alloca double, align 8
  %x1 = alloca double, align 8
  store double %x, ptr %x1, align 8
  store double 1.000000e+00, ptr %a, align 8
  store double 1.000000e+00, ptr %b, align 8
  store double 3.000000e+00, ptr %i, align 8
  br label %for_loop

for_loop:                                         ; preds = %for_loop, %fun_entry
  %i7 = phi double [ %for_counter_next, %for_loop ], [ 3.000000e+00, %fun_entry ]
  %b3 = phi double [ %add_temp, %for_loop ], [ 1.000000e+00, %fun_entry ]
  %a2 = phi double [ %b3, %for_loop ], [ 1.000000e+00, %fun_entry ]
  %add_temp = fadd double %a2, %b3
  store double %b3, ptr %a, align 8
  %op_seq_temp = call double @"operator:"(double %add_temp, double %b3)
  store double %add_temp, ptr %b, align 8
  %op_seq_temp6 = call double @"operator:"(double %op_seq_temp, double %add_temp)
  %less_than_temp = fcmp ult double %i7, %x
  %for_counter_next = fadd double %i7, 1.000000e+00
  store double %for_counter_next, ptr %i, align 8
  br i1 %less_than_temp, label %for_loop, label %for_post

for_post:                                         ; preds = %for_loop
  %op_seq_temp11 = call double @"operator:"(double 0.000000e+00, double %add_temp)
  ret double %op_seq_temp11
}

define double @__top_level_expr() {
fun_entry:
  %fun_call_temp = call double @fibi(double 1.000000e+01)
  ret double %fun_call_temp
}
