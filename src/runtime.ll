; ModuleID = 'runtime.ll'
; This file contains dmjit runtime support library bindings

%DMValue = type { i8, i32 }

declare external i8 @dmir.runtime.inc_ref_count(%DMValue)
declare external i8 @dmir.runtime.dec_ref_count(%DMValue)
declare external i8 @dmir.runtime.get_variable(%DMValue*, %DMValue, i32)
declare external i8 @dmir.runtime.set_variable(%DMValue, i32, %DMValue)
declare external i8 @dmir.runtime.call_proc_by_id(%DMValue*, %DMValue, i32, i32, i32, %DMValue, %DMValue*, i32, i32, i32)
declare external i8 @dmir.runtime.call_proc_by_name(%DMValue*, %DMValue, i32, i32, %DMValue, %DMValue*, i32, i32, i32)

declare external void @dmir.runtime.deopt(i64)

declare external void @dmir.runtime.debug.handle_debug(i8*)
declare external void @dmir.runtime.debug.handle_debug_val(%DMValue)

declare external %DMValue @dmir.runtime.list_indexed_get(%DMValue, i32)
declare external void     @dmir.runtime.list_indexed_set(%DMValue, i32, %DMValue)
declare external %DMValue @dmir.runtime.list_associative_get(%DMValue, %DMValue)
declare external void     @dmir.runtime.list_associative_set(%DMValue, %DMValue, %DMValue)
declare external %DMValue @dmir.runtime.list_copy(%DMValue)
declare external i1       @dmir.runtime.list_check_size(%DMValue, i32)
declare external void     @dmir.runtime.list_append(%DMValue, %DMValue)
declare external void     @dmir.runtime.list_remove(%DMValue, %DMValue)

declare external i1       @dmir.runtime.is_dm_entity(%DMValue)

; It should be guaranteed by caller, that on entry to this function, arg_count is at least caller_arg_count - 1
define void @dmir.intrinsic.unref_excess_arguments_internal(%DMValue* %args, i32 %arg_count, i32 %caller_arg_count) cold {
entry:
    br label %unref
unref:
    %arg_count_phi = phi i32 [ %arg_count, %entry ], [ %arg_count_inc, %unref ]
    %ptr = getelementptr %DMValue, %DMValue* %args, i32 %arg_count
    %value = load %DMValue, %DMValue* %ptr
    %unused = call i8 @dmir.runtime.dec_ref_count(%DMValue %value)
    %arg_count_inc = add i32 %arg_count_phi, 1
    ; %arg_count < %caller_arg_count
    %cond = icmp ult i32 %arg_count_inc, %caller_arg_count
    br i1 %cond, label %unref, label %post_unref
post_unref:
    ret void
}

define available_externally void @dmir.intrinsic.unref_excess_arguments(%DMValue* %args, i32 %arg_count, i32 %caller_arg_count) alwaysinline {
; if (%arg_count < %caller_arg_count) {
check:
    %cond = icmp ult i32 %arg_count, %caller_arg_count
    br i1 %cond, label %unref, label %post
; call_unref()
unref:
    call void @dmir.intrinsic.unref_excess_arguments_internal(%DMValue* %args, i32 %arg_count, i32 %caller_arg_count)
    br label %post
; }
post:
    ret void
}

