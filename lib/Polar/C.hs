{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Polar.C
  ( polar_new
  , polar_load
  , polar_clear_rules, polar_register_constant, polar_register_mro, polar_next_inline_query, polar_new_query_from_term, polar_new_query, polar_next_polar_mesage, polar_next_query_event, polar_debug_command, polar_call_result, polar_application_error, polar_question_result, polar_next_query_message, polar_query_surce_info, polar_bind, polar_get_external_id, polar_free, query_free, result_free) where

import Polar.C.Context ( polarCtx )
import Foreign.Ptr (Ptr, FunPtr)
import qualified Language.C.Inline as C
import Polar.C.Types ( Polar, CResult_c_void, Query, CResult_Query, CResult_c_char )
import Data.Word (Word64, Word32)
import Data.Int (Int32)

C.context (C.baseCtx <> polarCtx)


C.include "stdint.h"


C.include "polar.h"


polar_new :: IO (Ptr Polar)
polar_new = [C.exp| polar_Polar* { polar_new() } |]


polar_load :: Ptr Polar -> Ptr C.CChar -> IO (Ptr CResult_c_void)
polar_load polarPtr srcPtr =
  [C.exp| polar_CResult_c_void* { 
    polar_load(
      $(polar_Polar* polarPtr), 
      $(char* srcPtr)
    ) 
  } |]


polar_clear_rules :: Ptr Polar -> IO (Ptr CResult_c_void)
polar_clear_rules polarPtr = 
  [C.exp| polar_CResult_c_void* { 
    polar_clear_rules($(polar_Polar* polarPtr))
  } |]


polar_register_constant :: Ptr Polar -> Ptr C.CChar -> Ptr C.CChar -> IO (Ptr CResult_c_void)
polar_register_constant polarPtr namePtr valuePtr =
  [C.exp| polar_CResult_c_void* { 
    polar_register_constant(
      $(polar_Polar* polarPtr), 
      $(char* namePtr), 
      $(char* valuePtr)
    ) 
  } |]


polar_register_mro :: Ptr Polar -> Ptr C.CChar -> Ptr C.CChar -> IO (Ptr CResult_c_void)
polar_register_mro polarPtr namePtr valuePtr = 
  [C.exp| polar_CResult_c_void* { 
    polar_register_mro(
      $(polar_Polar* polarPtr), 
      $(char* namePtr), 
      $(char* valuePtr)
    ) 
  } |]


polar_next_inline_query :: Ptr Polar -> Word32 -> IO (Ptr Query)
polar_next_inline_query polarPtr trace =
    [C.exp| polar_Query* { 
      polar_next_inline_query(
        $(polar_Polar* polarPtr), 
        $(uint32_t trace)
      ) 
    } |]


polar_new_query_from_term :: Ptr Polar -> Ptr C.CChar -> Word32 -> IO (Ptr CResult_Query)
polar_new_query_from_term polarPtr queryPtr traceInt = do
  [C.exp| polar_CResult_Query* { 
    polar_new_query_from_term(
      $(polar_Polar* polarPtr), 
      $(char* queryPtr), 
      $(uint32_t traceInt)
    ) 
  } |]


polar_new_query :: Ptr Polar -> Ptr C.CChar -> Word32 -> IO (Ptr CResult_Query)
polar_new_query polarPtr queryPtr traceInt =
  [C.exp| polar_CResult_Query* { 
    polar_new_query(
      $(polar_Polar* polarPtr), 
      $(char* queryPtr), 
      $(uint32_t traceInt)
    ) 
  } |]


polar_next_polar_mesage :: Ptr Polar -> IO (Ptr CResult_c_char)
polar_next_polar_mesage polarPtr = 
  [C.exp| polar_CResult_c_char * {
    polar_next_polar_message($(polar_Polar* polarPtr))
  } |]


polar_next_query_event :: Ptr Query -> IO (Ptr CResult_c_char)
polar_next_query_event queryPtr =
  [C.exp| polar_CResult_c_char* { 
    polar_next_query_event($(polar_Query* queryPtr)) 
  } |]


polar_debug_command :: Ptr Query -> Ptr C.CChar -> IO (Ptr CResult_c_void)
polar_debug_command queryPtr valuePtr =
  [C.exp| polar_CResult_c_void* { 
    polar_debug_command(
      $(polar_Query* queryPtr),
      $(char* valuePtr)
    ) 
  } |]


polar_call_result :: Ptr Query -> Word64 -> Ptr C.CChar -> IO (Ptr CResult_c_void)
polar_call_result queryPtr callId termPtr =
  [C.exp| polar_CResult_c_void* { 
    polar_call_result(
      $(polar_Query* queryPtr), 
      $(uint64_t callId), 
      $(char *termPtr)
    ) 
  } |]


polar_question_result :: Ptr Query -> Word64 -> Int32 -> IO (Ptr CResult_c_void)
polar_question_result queryPtr callId yn =
  [C.exp| polar_CResult_c_void * {
    polar_question_result(
      $(polar_Query *queryPtr),
      $(uint64_t callId),
      $(int32_t yn)
    )
  } |]


polar_application_error :: Ptr Query -> Ptr C.CChar -> IO (Ptr CResult_c_void)
polar_application_error queryPtr messagePtr =
  [C.exp| polar_CResult_c_void * {
    polar_application_error(
      $(polar_Query *queryPtr),
      $(char* messagePtr)
    )
  } |]


polar_next_query_message :: Ptr Query -> IO (Ptr CResult_c_char)
polar_next_query_message queryPtr =
  [C.exp| polar_CResult_c_char* { 
    polar_next_query_message($(polar_Query* queryPtr)) 
  } |]


polar_query_surce_info :: Ptr Query -> IO (Ptr CResult_c_char)
polar_query_surce_info queryPtr =
  [C.exp| polar_CResult_c_char* { 
    polar_query_source_info($(polar_Query* queryPtr)) 
  } |]


polar_bind :: Ptr Query -> Ptr C.CChar -> Ptr C.CChar -> IO (Ptr CResult_c_void)
polar_bind queryPtr name value =
  [C.exp| polar_CResult_c_void* { 
    polar_bind(
      $(polar_Query* queryPtr),
      $(char* name),
      $(char* value)
    ) 
  } |]


polar_get_external_id :: Ptr Polar -> IO Word64
polar_get_external_id polarPtr =
  [C.exp| uint64_t { polar_get_external_id($(polar_Polar* polarPtr)) } |]


polar_free :: FunPtr (Ptr Polar -> IO ())
polar_free = [C.funPtr| void hs_polar_free(polar_Polar* polar) { polar_free(polar); } |]


query_free :: FunPtr (Ptr Query -> IO ())
query_free = 
  [C.funPtr| void hs_query_free(polar_Query* query) { query_free(query); } |]


result_free :: Ptr CResult_c_void -> IO Int32
result_free result = [C.exp| int32_t { result_free($(polar_CResult_c_void* result)) } |]
