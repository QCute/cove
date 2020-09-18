# Cove
* an erlang early return operator to write compact code like rust ? operator
* use case match clause convert flatten code to case tree code(form)

# quick start
* Add to rebar.config
```erlang
{deps, [
  ...
  {cove, {git, "https://github.com/QCute/cove.git", {branch, "master"}}}
]}.
```

* add parser transform 
```erlang
-include("cove.hrl")
```
or
```erlang
-compile({parse_transform, cove}).
```

* write code
```erlang
-spec a() -> ok | error.
a() ->
    ok.

-spec b() -> {ok, term()} | {error, term()}.
b() ->
    {ok, b}.

-spec c() -> {ok, term()} | {error, term()}.
c() ->
    {error, c}.

-record(record, {id, name}).
-spec d() -> #record{} | [].
d() ->
    #record{id = 0}.

-spec do() -> {ok, term()} | {error, term()} | [].
do() ->
    ok = a() or _,                          %% return call result if not match
    {ok, B} = b() or _,                     %% match B if success, return call result if not match
    {ok, C} = c() or {error, not_satisfy},  %% match C if success, return this result if not match
    Record = #record{id = 0} = d() or _,    %% match #record{} and id equals 0 if success, return this result if not match
    {ok, {B, C}, Record}.
```

* customize operator

add compile options 
```erlang
{cove_operator, '!'}
```

enjoy it~
