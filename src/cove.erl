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
    function_return(Forms, proplists:get_value(cove_operator, Options, 'or'), []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% function return
function_return([], _, Forms) ->
    lists:reverse(Forms);
function_return([{function, Line, Name, SubLine, [{clause, Line, Arguments, Padding, Clause}]} | T], Operator, Forms) ->
    NewClause = mark_return(lists:reverse(Clause), Operator, []),
    function_return(T, Operator, [{function, Line, Name, SubLine, [{clause, Line, Arguments, Padding, NewClause}]} | Forms]);
function_return([H | T], Operator, Forms) ->
    function_return(T, Operator, [H | Forms]).

%% mark return
mark_return([], _, Clause) ->
    Clause;
mark_return([{match, Line, Ok, {op, Line, Operator, Operation, {var, Line, '_'}}} | T], Operator, Clause) ->
    %% mark with default return
    CaseClause = [{'case', Line, Operation, [{'clause', Line, [Ok], [], Clause}, {'clause', Line, [{var, Line, 'Error'}], [], [{var, Line, 'Error'}]}]}],
    mark_return(T, Operator, CaseClause);
mark_return([{match, Line, Ok, {op, Line, Operator, Operation, Return}} | T], Operator, Clause) ->
    %% mark with return
    CaseClause = [{'case', Line, Operation, [{'clause', Line, [Ok], [], Clause}, {'clause', Line, [{var, Line, '_'}], [], [Return]}]}],
    mark_return(T, Operator, CaseClause);
mark_return([H | T], Operator, Clause) ->
    mark_return(T, Operator, [H | Clause]).
