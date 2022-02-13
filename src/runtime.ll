; ModuleID = 'runtime.ll'
; This file contains dmjit runtime support library bindings

%DMValue = type { i8, i32 }

declare i8 @"<intrinsic>/dec_ref_count"(%DMValue)
