%%%-------------------------------------------------------------------
%%% @doc
%%% cove
%%% * an erlang early return operator to write compact code like rust ? operator
%%% * use case match clause convert flatten code to case tree code(form)
%%% @end
%%%-------------------------------------------------------------------
-module(cove).
-export([parse_transform/2]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc implements the cove parse transform
-spec parse_transform(Forms :: [erl_parse:abstract_form() | erl_parse:form_info()], Options :: [compile:option()]) -> NewForms :: [erl_parse:abstract_form() | erl_parse:form_info()].
parse_transform(Forms, Options) ->
    handle_function_form(Forms, proplists:get_value(cove_operator, Options, 'or'), []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% handle function form
handle_function_form([], _, Forms) ->
    lists:reverse(Forms);
handle_function_form([{function, Line, Name, SubLine, [{clause, Line, Arguments, Padding, Clause}]} | T], Operator, Forms) ->
    %% find function clause
    NewClause = handle_flat_match_form(lists:reverse(Clause), Operator, [], []),
    %% rebuilt function flat match clause to tree case clause
    handle_function_form(T, Operator, [{function, Line, Name, SubLine, [{clause, Line, Arguments, Padding, NewClause}]} | Forms]);
handle_function_form([H | T], Operator, Forms) ->
    %% other form
    handle_function_form(T, Operator, [H | Forms]).

%% handle flat match form
handle_flat_match_form([], _, _, Clause) ->
    Clause;
handle_flat_match_form([{match, Line, Ok, {op, Line, Operator, Operation, {var, Line, '_'}}} | T], Operator, [], Clause) ->
    %% mark with default return
    CaseClause = [{'case', Line, Operation, [{'clause', Line, [Ok], [], Clause}, {'clause', Line, [{var, Line, 'Error'}], [], [{var, Line, 'Error'}]}]}],
    handle_flat_match_form(T, Operator, [], CaseClause);
handle_flat_match_form([{match, Line, Ok, {op, Line, Operator, Operation, Return}} | T], Operator, [], Clause) ->
    %% mark with return
    CaseClause = [{'case', Line, Operation, [{'clause', Line, [Ok], [], Clause}, {'clause', Line, [{var, Line, '_'}], [], [Return]}]}],
    handle_flat_match_form(T, Operator, [], CaseClause);
handle_flat_match_form([{match, Line, Ok, {op, Line, Operator, Operation, {var, Line, '_'}}} | T], Operator, Stack, Clause) ->
    %% with stack match
    SubClause = lists:foldl(fun(Match, Acc) -> {match, Line, Match, Acc} end, Ok, Stack),
    %% mark with default return
    CaseClause = [{'case', Line, Operation, [{'clause', Line, [SubClause], [], Clause}, {'clause', Line, [{var, Line, 'Error'}], [], [{var, Line, 'Error'}]}]}],
    handle_flat_match_form(T, Operator, [], CaseClause);
handle_flat_match_form([{match, Line, Ok, {op, Line, Operator, Operation, Return}} | T], Operator, Stack, Clause) ->
    %% with stack match
    SubClause = lists:foldl(fun(Match, Acc) -> {match, Line, Match, Acc} end, Ok, Stack),
    %% mark with return
    CaseClause = [{'case', Line, Operation, [{'clause', Line, [SubClause], [], Clause}, {'clause', Line, [{var, Line, '_'}], [], [Return]}]}],
    handle_flat_match_form(T, Operator, [], CaseClause);
handle_flat_match_form([{match, _, Ok, SubMatch = {match, _, _, _}} | T], Operator, Stack, Clause) ->
    %% with stack match clause
    CaseClause = handle_flat_match_form([SubMatch], Operator, [Ok | Stack], Clause),
    handle_flat_match_form(T, Operator, [], CaseClause);
handle_flat_match_form([H | T], Operator, Stack, Clause) ->
    %% normal clause
    handle_flat_match_form(T, Operator, Stack, [H | Clause]).
