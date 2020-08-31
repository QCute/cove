# Cove
* an erlang early return operator to write compact code like rust ? operator
* use case match clause convert flatten code to case tree code(form)

# quick start
* Add to rebar.config
```
{deps, [
  ...
  {cove, {git, "https://github.com/QCute/cove.git", {branch, "master"}}}
]}.
```

* add parser transform 
```
-include("cove.hrl")
```
or
```
-compile({parse_transform, cove}).
```

* write code
```
-spec a() -> ok | error.
a() ->
    ok.

-spec b() -> {ok, term()} | {error, term()}.
b() ->
    {ok, b}.

-spec c() -> {ok, term()} | {error, term()}.
c() ->
    {error, c}.

-spec do() -> {ok, term()} | {error, term()}.
do() ->
    ok = ?MODULE:a() or _,                          %% return call result if not match
    {ok, B} = ?MODULE:b() or _,                     %% match B if success, return call result if not match
    {ok, C} = ?MODULE:c() or {error, not_satisfy},  %% match C if success, return this result if not match
    {ok, {B, C}}.
```

* customize operator

add compile options 
```
{cove_operator, '!'}
```

enjoy it~
