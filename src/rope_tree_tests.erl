-module(rope_tree_tests).

-include_lib("proper/include/proper.hrl").

-include("rope_tree.hrl").

-export([ gen_tree/0
        ]).


%%%
%%% Properties
%%%

prop_nth() ->
    ?FORALL(T, gen_non_empty_tree(),
            ?FORALL(I, gen_valid_position(T),
                    begin
                        TreeChar = rope_tree:nth(T, I),
                        StringChar = lists:nth(I, rope_tree:to_string(T)),
                        TreeChar =:= StringChar
                    end
                   )).

prop_length() ->
    ?FORALL(T, gen_tree(),
            begin
                TreeLength = rope_tree:length(T),
                StringLength = erlang:length(rope_tree:to_string(T)),
                TreeLength =:= StringLength
            end
           ).

prop_concatenate() ->
    ?FORALL({T1, T2}, {gen_tree(), gen_tree()},
            begin
                Trees = rope_tree:concatenate(T1, T2),
                Strings = rope_tree:to_string(T1) ++ rope_tree:to_string(T2),
                Strings =:= rope_tree:to_string(Trees)
            end
           ).

prop_split_concatenate() ->
    ?FORALL(T, gen_non_empty_tree(),
           ?FORALL(I, gen_valid_index(T),
                  begin
                      OriginalString = rope_tree:to_string(T),
                      {T1, T2} = rope_tree:split(T, I),
                      NewTree = rope_tree:concatenate(T1, T2),
                      OriginalString =:= rope_tree:to_string(NewTree)
                  end
                  )).

prop_split_strings() ->
    ?FORALL(T, gen_non_empty_tree(),
           ?FORALL(I, gen_valid_index(T),
                  begin
                      OriginalString = rope_tree:to_string(T),
                      {T1, T2} = rope_tree:split(T, I),
                      S1 = rope_tree:to_string(T1),
                      S2 = rope_tree:to_string(T2),
                      {S1, S2} =:= lists:split(I, OriginalString)
                  end
                  )).

prop_insert_string() ->
    ?FORALL({T, S}, {gen_non_empty_tree(), string()},
            ?FORALL(I, gen_valid_index(T),
                    begin
                        {S1, S2} = lists:split(I, rope_tree:to_string(T)),
                        NewString = S1 ++ S ++ S2,
                        NewTree = rope_tree:insert_string(T, I, S),
                        NewString =:= rope_tree:to_string(NewTree)
                    end
                   )).

prop_insert_character() ->
    ?FORALL({T, C}, {gen_non_empty_tree(), char()},
            ?FORALL(I, gen_valid_index(T),
                    begin
                        {S1, S2} = lists:split(I, rope_tree:to_string(T)),
                        NewString = S1 ++ [C | S2],
                        NewTree = rope_tree:insert_character(T, I, C),
                        NewString =:= rope_tree:to_string(NewTree)
                    end
                   )).

prop_delete_seq() ->
    ?FORALL(T, gen_non_empty_tree(),
           ?FORALL(End, gen_valid_index(T),
                   ?FORALL(Start, integer(0, End),
                           begin
                               OriginalString = rope_tree:to_string(T),
                               {S1, S2} = lists:split(End, OriginalString),
                               {S0, _S} = lists:split(Start, S1),
                               NewString = S0 ++ S2,
                               NewTree = rope_tree:delete_seq(T, Start, End),
                               NewString =:= rope_tree:to_string(NewTree)
                           end
                          ))).

prop_delete_character() ->
    ?FORALL(T, gen_non_empty_tree(),
           ?FORALL(P, gen_valid_position(T),
                   begin
                       OriginalString = rope_tree:to_string(T),
                       {S1, [_ | S2]} = lists:split(P - 1, OriginalString),
                       NewString = S1 ++ S2,
                       NewTree = rope_tree:delete_character(T, P),
                       NewString =:= rope_tree:to_string(NewTree)
                   end
                  )).

prop_rotation_is_symmetric() ->
    ?FORALL(T, gen_complex_tree(),
            begin
                RotatedLeft = rope_tree:rotate_left(T),
                T =:= rope_tree:rotate_right(RotatedLeft)
            end
           ).

prop_left_rotation_preserves_order() ->
    ?FORALL(T, gen_complex_tree(),
            begin
                OriginalString = rope_tree:to_string(T),
                RotatedLeft = rope_tree:rotate_left(T),
                OriginalString =:= rope_tree:to_string(RotatedLeft)
            end
           ).

prop_right_rotation_preserves_order() ->
    ?FORALL(T, gen_complex_tree(),
            begin
                OriginalString = rope_tree:to_string(T),
                RotatedRight = rope_tree:rotate_right(T),
                OriginalString =:= rope_tree:to_string(RotatedRight)
            end
           ).

prop_rebalance_preserves_order() ->
    ?FORALL(Strictness, integer(1, 3),
            ?FORALL(T, gen_unbalanced_tree(Strictness),
                    begin
                        OriginalString = rope_tree:to_string(T),
                        {RT, _D, _B} = rope_tree:rebalance(T, Strictness),
                        OriginalString =:= rope_tree:to_string(RT)
                    end
                   )).

prop_rebalance_does_not_increase_depth() ->
    ?FORALL(Strictness, integer(1, 3),
            ?FORALL(T, gen_tree(),
                    begin
                        OldDepth = rope_tree:depth(T),
                        {_RT, NewDepth, _Balance} =
                            rope_tree:rebalance(T, Strictness),
                        abs(NewDepth) =< abs(OldDepth)
                    end
                   )).


%%%
%%% Generators
%%%

gen_tree() ->
    ?LAZY(
       ?LET({Left, Right},
            weighted_union([ {2, {string(), string()}}
                           , {1, {string(), gen_tree()}}
                           , {1, {gen_tree(), string()}}
                           , {1, {gen_tree(), gen_tree()}}
                           ]),
            ?NODE(rope_tree:length(Left), Left, Right)
           )).

gen_non_empty_tree() ->
    ?SUCHTHAT(?NODE(W, _L, R), gen_tree(), W =/= 0 andalso R =/= "").

gen_complex_tree() ->
    ?LET({T1, T2}, {gen_non_empty_tree(), gen_non_empty_tree()},
         rope_tree:concatenate(T1, T2)).

gen_unbalanced_tree(Strictness) ->
    ?SUCHTHAT(Tree, ?LET({T1, T2}, {gen_complex_tree(), gen_complex_tree()},
                         rope_tree:concatenate(T1, T2)),
              not rope_tree:is_balanced(Tree, Strictness)).

gen_valid_index(Tree) ->
    integer(0, rope_tree:length(Tree)).

gen_valid_position(Tree) ->
    integer(1, rope_tree:length(Tree)).
