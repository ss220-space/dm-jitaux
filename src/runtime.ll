; ModuleID = 'runtime.ll'
; This file contains dmjit runtime support library bindings

%DMValue = type { i8, i32 }
%List = type { %DMValue*, i8*, i32, i32, i32, i8* }

@dmir.runtime.GLOB_LIST_ARRAY = external global %List**, align 4

declare external i8 @dmir.runtime.inc_ref_count(%DMValue)
declare external i8 @dmir.runtime.dec_ref_count(%DMValue)
declare external i8 @dmir.runtime.get_variable(%DMValue*, %DMValue, i32)
declare external i8 @dmir.runtime.set_variable(%DMValue, i32, %DMValue)
declare external i8 @dmir.runtime.call_proc_by_id(%DMValue*, %DMValue, i32, i32, i32, %DMValue, %DMValue*, i32, i32, i32)
declare external i8 @dmir.runtime.call_proc_by_name(%DMValue*, %DMValue, i32, i32, %DMValue, %DMValue*, i32, i32, i32)

declare external void @dmir.runtime.deopt(i64)

declare external void @dmir.runtime.debug.handle_debug(i8*)
declare external void @dmir.runtime.debug.handle_debug_val(%DMValue)

declare external void     @dmir.runtime.unset_assoc_list(i8**, %DMValue)
declare external %DMValue @dmir.runtime.list_associative_get(%DMValue, %DMValue)
declare external void     @dmir.runtime.list_associative_set(%DMValue, %DMValue, %DMValue)
declare external %DMValue @dmir.runtime.list_copy(%DMValue)
declare external void     @dmir.runtime.list_append(%DMValue, %DMValue)
declare external void     @dmir.runtime.list_remove(%DMValue, %DMValue)
declare external i32      @dmir.runtime.create_new_list(i32)

declare external i1       @dmir.runtime.is_dm_entity(%DMValue)
declare external i1       @dmir.runtime.is_subtype_of(%DMValue, %DMValue)

declare external %DMValue @dmir.runtime.get_step(%DMValue, i8)

; iterator_type = 0, iterator_array = 1, iterator_allocated = 2, iterator_length = 3, iterator_index = 4, iterator_filter_flags = 5, iterator_filter_type = 6
%DMIterator = type { i8, %DMValue*, i32, i32, i32, i32, %DMValue }

; list copy passed here
declare external void @dmir.runtime.iter.load_array_from_list(%DMValue %list, %DMValue** %out_array, i32* %out_array_size, i32* %out_length)

; bridge to ensure llvm see constant values of iterator_type and so on
define available_externally void @dmir.intrinsic.iter.load_array_from_list(%DMIterator* noalias nocapture sret(%DMIterator) %res, %DMValue %list, i32 %filter_bitmask) alwaysinline {
    %array_ptr = alloca %DMValue*, align 8
    %array_size_ptr = alloca i32, align 8
    %length_ptr = alloca i32, align 8
    call void @dmir.runtime.iter.load_array_from_list(%DMValue %list, %DMValue** %array_ptr, i32* %array_size_ptr, i32* %length_ptr)
    %array = load %DMValue*, %DMValue** %array_ptr
    %array_size = load i32, i32* %array_size_ptr
    %length = load i32, i32* %length_ptr
    %iter_1 = insertvalue %DMIterator { i8 5, %DMValue* undef, i32 undef, i32 undef, i32 0, i32 undef, %DMValue zeroinitializer }, %DMValue* %array, 1
    %iter_2 = insertvalue %DMIterator %iter_1, i32 %array_size, 2
    %iter_3 = insertvalue %DMIterator %iter_2, i32 %length, 3
    %iter_4 = insertvalue %DMIterator %iter_3, i32 %filter_bitmask, 5
    store %DMIterator %iter_4, %DMIterator* %res
    ret void
}

declare external void @dmir.runtime.iter.load_array_from_object(%DMValue %obj, %DMValue** %out_array, i32* %out_array_size, i32* %out_length, i32 %bitmask)

define available_externally void @dmir.intrinsic.iter.load_array_from_object(%DMIterator* noalias nocapture sret(%DMIterator) %res, %DMValue %obj, i32 %filter_bitmask) alwaysinline {
    %array_ptr = alloca %DMValue*, align 8
    %array_size_ptr = alloca i32, align 8
    %length_ptr = alloca i32, align 8
    call void @dmir.runtime.iter.load_array_from_object(%DMValue %obj, %DMValue** %array_ptr, i32* %array_size_ptr, i32* %length_ptr, i32 %filter_bitmask)
    %array = load %DMValue*, %DMValue** %array_ptr
    %array_size = load i32, i32* %array_size_ptr
    %length = load i32, i32* %length_ptr
    %iter_1 = insertvalue %DMIterator { i8 7, %DMValue* undef, i32 undef, i32 undef, i32 0, i32 undef, %DMValue zeroinitializer }, %DMValue* %array, 1
    %iter_2 = insertvalue %DMIterator %iter_1, i32 %array_size, 2
    %iter_3 = insertvalue %DMIterator %iter_2, i32 %length, 3
    %iter_4 = insertvalue %DMIterator %iter_3, i32 %filter_bitmask, 5
    store %DMIterator %iter_4, %DMIterator* %res
    ret void
}

define available_externally i1 @dmir.intrinsic.iter.filter_mask(i32 %bitmask, %DMValue %value) alwaysinline {
    %no_bits_set = icmp eq i32 %bitmask, 0
    %any_bit_value = and i32 %bitmask, 4096
    %any_bit_set = icmp ne i32 %any_bit_value, 0
    %accept_any = or i1 %no_bits_set, %any_bit_set
    br i1 %accept_any, label %allow_any, label %check_tag
allow_any:
    ret i1 1
check_tag:
    %tag = extractvalue %DMValue %value, 0
    switch i8 %tag, label %result [
        i8 0,  label %check_null
        i8 1,  label %check_turf
        i8 2,  label %check_obj
        i8 3,  label %check_mob
        i8 4,  label %check_area
        i8 6,  label %check_string
        i8 12, label %check_resource
        i8 42, label %check_number
    ]
check_null:
    br label %result
check_turf:
    br label %result
check_obj:
    br label %result
check_mob:
    br label %result
check_area:
    br label %result
check_string:
    br label %result
check_resource:
    br label %result
check_number:
    br label %result
result:
    %value_bits = phi i32
        [ 128, %check_null ],
        [ 32,  %check_turf ],
        [ 2,   %check_obj ],
        [ 1,   %check_mob ],
        [ 256, %check_area ],
        [ 4,   %check_string ],
        [ 16,  %check_resource ],
        [ 8,   %check_number ],
        [ 0,   %check_tag ]
    %allow_bits = and i32 %bitmask, %value_bits
    %is_pass = icmp ne i32 %allow_bits, 0
    ret i1 %is_pass
}

define available_externally { %DMValue, i1 } @dmir.intrinsic.iter.next(%DMIterator* nocapture noalias %iterator_ptr) alwaysinline {
entry:
    %iterator = load %DMIterator, %DMIterator* %iterator_ptr
    %iter_type = extractvalue %DMIterator %iterator, 0
    %array_ptr = extractvalue %DMIterator %iterator, 1
    %iter_length = extractvalue %DMIterator %iterator, 3
    %iter_filter_kind = extractvalue %DMIterator %iterator, 5

    switch i8 %iter_type, label %exit_no_next [
        i8 5, label %iter_type_5_entry
        i8 7, label %iter_type_5_entry
        i8 8, label %iter_type_5_entry
    ]
iter_type_5_entry:
    %iter_index_ptr = getelementptr %DMIterator, %DMIterator* %iterator_ptr, i32 0, i32 4
    %iter_index = load i32, i32* %iter_index_ptr
    %cond = icmp ult i32 %iter_index, %iter_length
    br i1 %cond, label %iter_type_5_load_next, label %exit_no_next
iter_type_5_load_next:
    %value_ptr = getelementptr %DMValue, %DMValue* %array_ptr, i32 %iter_index
    %value = load %DMValue, %DMValue* %value_ptr
    %matches = call i1 @dmir.intrinsic.iter.filter_mask(i32 %iter_filter_kind, %DMValue %value)
    %iter_index_inc = add i32 %iter_index, 1
    store i32 %iter_index_inc, i32* %iter_index_ptr
    br i1 %matches, label %exit_has_next, label %iter_type_5_entry
exit_has_next:
    %res_value = phi %DMValue [ %value, %iter_type_5_load_next ]
    %packed_1 = insertvalue { %DMValue, i1 } { %DMValue zeroinitializer, i1 1 }, %DMValue %res_value, 0
    ret { %DMValue, i1 } %packed_1
exit_no_next:
    ret { %DMValue, i1 } { %DMValue zeroinitializer, i1 0 }
}

declare external void @dmir.runtime.iter.unref(%DMValue* %array, i32 %length)
declare external void @dmir.runtime.iter.free(%DMValue* %array)

define available_externally void @dmir.intrinsic.iter.destroy(%DMIterator* %iterator_ptr) alwaysinline {
    %is_not_null = icmp ne %DMIterator* %iterator_ptr, null
    br i1 %is_not_null, label %non_null, label %is_null
non_null:
    %iterator = load %DMIterator, %DMIterator* %iterator_ptr
    %array = extractvalue %DMIterator %iterator, 1
    %array_length = extractvalue %DMIterator %iterator, 3
    call void @dmir.runtime.iter.unref(%DMValue* %array, i32 %array_length)
    call void @dmir.runtime.iter.free(%DMValue* %array)
    %filter_type_value = extractvalue %DMIterator %iterator, 6
    call i8 @dmir.runtime.dec_ref_count(%DMValue %filter_type_value)
    ret void
is_null:
    ret void
}

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


define %List* @dmir.intrinsic.get_list(%DMValue %list_id) alwaysinline {
entry:
    %id = extractvalue %DMValue %list_id, 1
    %glob_list = load %List**, %List*** @dmir.runtime.GLOB_LIST_ARRAY, align 4
    %array_element = getelementptr inbounds %List*, %List** %glob_list, i32 %id
    %list_ptr = load %List*, %List** %array_element, align 4

    ret %List* %list_ptr
}

define %DMValue* @dmir.intrinsic.get_list_vector_part(%DMValue %list_id) alwaysinline {
entry:
    %list_ptr = call %List* @dmir.intrinsic.get_list(%DMValue %list_id)

    %vector_part_ptr = getelementptr inbounds %List, %List* %list_ptr, i32 0, i32 0
    %vector_part = load %DMValue*, %DMValue** %vector_part_ptr, align 4

    ret %DMValue* %vector_part
}

define %DMValue @dmir.intrinsic.list_indexed_get(%DMValue %list_id, i32 %index) alwaysinline {
entry:
    %vector_part = call %DMValue* @dmir.intrinsic.get_list_vector_part(%DMValue %list_id)

    %index_dec = sub i32 %index, 1
    %array_element = getelementptr inbounds %DMValue, %DMValue* %vector_part, i32 %index_dec
    %ret = load %DMValue, %DMValue* %array_element, align 4

    ret %DMValue %ret
}

define void @dmir.intrinsic.list_indexed_set(%DMValue %list_id, i32 %index, %DMValue %value) alwaysinline {
entry:
    %list_ptr = call %List* @dmir.intrinsic.get_list(%DMValue %list_id)

    %vector_part_ptr = getelementptr inbounds %List, %List* %list_ptr, i32 0, i32 0
    %assoc_part = getelementptr inbounds %List, %List* %list_ptr, i32 0, i32 1
    %vector_part = load %DMValue*, %DMValue** %vector_part_ptr, align 4
    %index_dec = sub i32 %index, 1
    %array_element = getelementptr inbounds %DMValue, %DMValue* %vector_part, i32 %index_dec
    %prev = load %DMValue, %DMValue* %array_element, align 4
    %unused = call i8 @dmir.runtime.dec_ref_count(%DMValue %prev)
    call void @dmir.runtime.unset_assoc_list(i8** %assoc_part, %DMValue %prev)
    store %DMValue %value, %DMValue* %array_element, align 4

    ret void
}

define void @dmir.intrinsic.list_indexed_set_internal(%DMValue* %vector_part, i32 %index, %DMValue %value) alwaysinline {
entry:
    %array_element = getelementptr inbounds %DMValue, %DMValue* %vector_part, i32 %index
    store %DMValue %value, %DMValue* %array_element, align 4

    ret void
}

define i32 @dmir.intrinsic.list_size(%DMValue %list_id) alwaysinline {
entry:
    %list_ptr = call %List* @dmir.intrinsic.get_list(%DMValue %list_id)

    %len_ptr = getelementptr inbounds %List, %List* %list_ptr, i32 0, i32 3
    %len = load i32, i32* %len_ptr, align 4

    ret i32 %len
}

define i1 @dmir.intrinsic.list_check_size(%DMValue %list_id, i32 %index) alwaysinline {
entry:
    %len = call i32 @dmir.intrinsic.list_size(%DMValue %list_id)

    %gt_zero = icmp sgt i32 %index, 0
    %lt_size = icmp sle i32 %index, %len
    %ret = and i1 %gt_zero, %lt_size

    ret i1 %ret
}