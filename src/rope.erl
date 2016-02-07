-module(rope).

-include("rope.hrl").

-export([ new/0
        , new/1
        , new/2
        , to_iolist/1
        , to_string/1
        , nth/2
        , length/1
        , concatenate/2
        , split/2
        , insert_string/3
        , insert_character/3
        , delete_seq/3
        , delete_character/2
        ]).


new() ->
    new("", 1).

new(String) ->
    new(String, 1).

new(String, Strictness) ->
    #rope{ length = erlang:length(String)
         , strictness = Strictness
         , tree = String
         }.


to_iolist(#rope{ tree = Tree }) ->
    rope_tree:to_iolist(Tree).


to_string(#rope{ tree = Tree }) ->
    rope_tree:to_string(Tree).


nth(#rope{ tree = Tree }, N) ->
    rope_tree:nth(Tree, N).


length(#rope{ length = Length }) ->
    Length.


concatenate(R1 = #rope{}, R2 = #rope{}) ->
    UnbalancedTree = rope_tree:concatenate(R1#rope.tree, R2#rope.tree),
    Strictness = max(R1#rope.strictness, R2#rope.strictness),
    {BalancedTree, _D, _B} = rope_tree:rebalance(UnbalancedTree, Strictness),
    #rope{ length = R1#rope.length + R2#rope.length
         , strictness = Strictness
         , tree = BalancedTree
         }.


split(R = #rope{ length = Length, tree = Tree, strictness = Strictness }, N) ->
    {T1, T2} = rope_tree:split(Tree, N),
    {BalancedT1, _D1, _B1} = rope_tree:rebalance(T1, Strictness),
    {BalancedT2, _D2, _B2} = rope_tree:rebalance(T2, Strictness),
    { R#rope{ length = N
            , tree = BalancedT1
            }
    , R#rope{ length = Length - N
            , tree = BalancedT2
            }
    }.


insert_string(R = #rope{ length = Length, tree = Tree }, N, String) ->
    R#rope{ length = Length + erlang:length(String)
          , tree = rope_tree:insert_string(Tree, N, String)
          }.


insert_character(R = #rope{ length = Length, tree = Tree }, N, Char) ->
    R#rope{ length = Length + 1
          , tree = rope_tree:insert_character(Tree, N, Char)
          }.


delete_seq(R = #rope{ length = Length, tree = Tree }, Start, End) ->
    R#rope{ length = Length - End + Start
          , tree = rope_tree:delete_seq(Tree, Start, End)
          }.


delete_character(R = #rope{ length = Length, tree = Tree }, N) ->
    R#rope{ length = Length - 1
          , tree = rope_tree:delete_character(Tree, N)
          }.
