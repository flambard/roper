-module(rope_tree).

-include("rope_tree.hrl").

-export([ to_iolist/1
        , to_string/1
        , nth/2
        , length/1
        , concatenate/2
        , split/2
        , insert_string/3
        , insert_character/3
        , delete_seq/3
        , delete_character/2
        , rotate_left/1
        , rotate_right/1
        , is_balanced/1
        , is_balanced/2
        , shallow_balance/1
        , depth/1
        , rebalance/1
        , rebalance/2
        , print/1
        ]).


to_iolist(?NODE(_W, Left, Right)) ->
    [to_iolist(Left) | to_iolist(Right)];
to_iolist(String) ->
    String.


to_string(Tree) ->
    lists:flatten(to_iolist(Tree)).


nth(?NODE(Weight, _L, Right), N) when Weight < N ->
    nth(Right, N - Weight);
nth(?NODE(_W, Left, _R), N) ->
    nth(Left, N);
nth(String, N) ->
    lists:nth(N, String).


length(?NODE(Weight, _L, Right)) ->
    Weight + rope_tree:length(Right);
length(String) ->
    erlang:length(String).


concatenate(Tree1, Tree2) ->
    ?NODE( rope_tree:length(Tree1)
         , Tree1
         , Tree2
         ).


split(T, 0) ->
    {"", T};
split(?NODE(W, Left, Right), N) when N < W ->
    %% Weight is larger than N: split left side
    {T1, T2} = split(Left, N),
    {?NODE(N, T1, ""), concatenate(T2, Right)};
split(?NODE(W, Left, Right), N) ->
    %% Weight is smaller than or equal to N: split right side
    {T1, T2} = split(Right, N - W),
    {?NODE(W, Left, T1), T2};
split(String, N) ->
    lists:split(N, String).


insert_string(Tree, N, String) ->
    {T1, T2} = split(Tree, N),
    concatenate(T1, concatenate(String, T2)).


insert_character(Tree, N, Character) ->
    insert_string(Tree, N, [Character]).


delete_seq(Tree, Start, End) ->
    {T1, T2} = split(Tree, End),
    {T0, _Deleted} = split(T1, Start),
    concatenate(T0, T2).


delete_character(Tree, N) ->
    delete_seq(Tree, N - 1, N).


rotate_left(?NODE(AW, A, ?NODE(BW, B, C))) ->
    ?NODE(AW + BW, ?NODE(AW, A, B), C).

rotate_right(?NODE(ABW, ?NODE(AW, A, B), C)) ->
    ?NODE(AW, A, ?NODE(ABW - AW, B, C)).


is_balanced(Tree) ->
    is_balanced(Tree, 1).

is_balanced(Node, Strictness) ->
    {Balance, _Depth} = balance(Node, Strictness),
    Balance.


shallow_balance(?NODE(_W, Left, Right)) ->
    depth(Left) - depth(Right);
shallow_balance(_String) ->
    0.


depth(?NODE(_W, Left, Right)) ->
    1 + max(depth(Left), depth(Right));
depth(_String) ->
    0.


rebalance(Tree) ->
    rebalance(Tree, 1).

rebalance(?NODE(_W, Left, Right), Strictness) ->
    {LT, LD, LB} = rebalance(Left, Strictness),
    {RT, RD, RB} = rebalance(Right, Strictness),
    Balance = LD - RD,
    if Balance > Strictness ->
            %% Node is left-heavy
            %% IF left child is right-heavy THEN
            %% - Left-rotate it
            %% FINALLY
            %% - Right-rotate the node
            RotatedLT =
                if LB < -Strictness -> rotate_left(LT);
                   true             -> LT
                end,
            Tree = rotate_right(concatenate(RotatedLT, RT)),
            Depth = depth(Tree),
            NewBalance = shallow_balance(Tree),
            {Tree, Depth, NewBalance};
       Balance < -Strictness ->
            %% Node is right-heavy
            %% IF right child is left-heavy THEN
            %% - Right-rotate it
            %% FINALLY
            %% - Left-rotate the node
            RotatedRT =
                if RB > Strictness -> rotate_right(RT);
                   true            -> RT
                end,
            Tree = rotate_left(concatenate(LT, RotatedRT)),
            Depth = depth(Tree),
            NewBalance = shallow_balance(Tree),
            {Tree, Depth, NewBalance};
       true ->
            %% Node is balanced
            Tree = concatenate(LT, RT),
            Depth = 1 + max(LD, RD),
            {Tree, Depth, Balance}
    end;
rebalance(String, _Strictness) ->
    {String, 0, 0}.


print(Tree) ->
    print(Tree, 0).

print(?NODE(W, Left, Right), Level) ->
    io:format("~s~w~n", [lists:duplicate(Level, $ ), W]),
    print(Left, Level + 2),
    print(Right, Level + 2);
print(String, Level) ->
    io:format("~s~w~n", [lists:duplicate(Level, $ ), String]).


%%%
%%% Internal functions
%%%

balance(?NODE(_W, Left, Right), Strictness) ->
    {LB, LD} = balance(Left, Strictness),
    {RB, RD} = balance(Right, Strictness),
    Balanced = LB and RB andalso Strictness >= abs(LD - RD),
    Depth = 1 + max(LD, RD),
    {Balanced, Depth};
balance(_String, _Strictness) ->
    {true, 0}.
