input: days:
let
  inherit (builtins) elemAt tail fromJSON foldl';
  growth' = d: data:
    if d == 0 then elemAt data 8
    else growth' (d - 1) ((tail data) ++ [(elemAt data 0 + elemAt data 2)]);
  growth = d: n: growth' (d - n) [1 1 1 1 1 1 1 1 1];
  initialState = fromJSON ("[" + input + "]");
  finalState = map (growth days) initialState;
  sum = foldl' (a: b: a + b) 0;
in sum finalState
