-module(rope_tests).

-include_lib("proper/include/proper.hrl").

-include("rope.hrl").

-export([ gen_rope/0
        ]).


%%%
%%% Properties
%%%

prop_new() ->
    ?FORALL(String, string(),
            String =:= rope:to_string(rope:new(String))
           ).

prop_nth() ->
    ?FORALL(T, gen_non_empty_rope(),
            ?FORALL(I, gen_valid_position(T),
                    begin
                        RopeChar = rope:nth(T, I),
                        StringChar = lists:nth(I, rope:to_string(T)),
                        RopeChar =:= StringChar
                    end
                   )).

prop_length() ->
    ?FORALL(R, gen_rope(),
            begin
                RopeLength = rope:length(R),
                StringLength = erlang:length(rope:to_string(R)),
                RopeLength =:= StringLength
            end
           ).

prop_concatenate() ->
    ?FORALL({R1, R2}, {gen_rope(), gen_rope()},
            begin
                Ropes = rope:concatenate(R1, R2),
                Strings = rope:to_string(R1) ++ rope:to_string(R2),
                Strings =:= rope:to_string(Ropes)
            end
           ).

prop_split_concatenate() ->
    ?FORALL(R, gen_non_empty_rope(),
           ?FORALL(I, gen_valid_index(R),
                  begin
                      OriginalString = rope:to_string(R),
                      {R1, R2} = rope:split(R, I),
                      NewRope = rope:concatenate(R1, R2),
                      OriginalString =:= rope:to_string(NewRope)
                  end
                  )).

prop_split_strings() ->
    ?FORALL(R, gen_non_empty_rope(),
           ?FORALL(I, gen_valid_index(R),
                  begin
                      OriginalString = rope:to_string(R),
                      {R1, R2} = rope:split(R, I),
                      S1 = rope:to_string(R1),
                      S2 = rope:to_string(R2),
                      {S1, S2} =:= lists:split(I, OriginalString)
                  end
                  )).

prop_insert_string() ->
    ?FORALL({R, S}, {gen_non_empty_rope(), string()},
            ?FORALL(I, gen_valid_index(R),
                    begin
                        {S1, S2} = lists:split(I, rope:to_string(R)),
                        NewString = S1 ++ S ++ S2,
                        NewRope = rope:insert_string(R, I, S),
                        NewString =:= rope:to_string(NewRope)
                    end
                   )).

prop_insert_character() ->
    ?FORALL({R, C}, {gen_non_empty_rope(), char()},
            ?FORALL(I, gen_valid_index(R),
                    begin
                        {S1, S2} = lists:split(I, rope:to_string(R)),
                        NewString = S1 ++ [C | S2],
                        NewRope = rope:insert_character(R, I, C),
                        NewString =:= rope:to_string(NewRope)
                    end
                   )).

prop_delete_seq() ->
    ?FORALL(R, gen_non_empty_rope(),
           ?FORALL(End, gen_valid_index(R),
                   ?FORALL(Start, integer(0, End),
                           begin
                               OriginalString = rope:to_string(R),
                               {S1, S2} = lists:split(End, OriginalString),
                               {S0, _S} = lists:split(Start, S1),
                               NewString = S0 ++ S2,
                               NewRope = rope:delete_seq(R, Start, End),
                               NewString =:= rope:to_string(NewRope)
                           end
                          ))).

prop_delete_character() ->
    ?FORALL(R, gen_non_empty_rope(),
           ?FORALL(P, gen_valid_position(R),
                   begin
                       OriginalString = rope:to_string(R),
                       {S1, [_ | S2]} = lists:split(P - 1, OriginalString),
                       NewString = S1 ++ S2,
                       NewRope = rope:delete_character(R, P),
                       NewString =:= rope:to_string(NewRope)
                   end
                  )).



%%%
%%% Generators
%%%

gen_rope() ->
    ?LET({Tree, Strictness}, {rope_tree_tests:gen_tree(), gen_strictness()},
         #rope{ length = rope_tree:length(Tree)
              , strictness = Strictness
              , tree = Tree
              }
        ).

gen_strictness() ->
    ?LET(I, int(), 1 + abs(I)).

gen_non_empty_rope() ->
    ?SUCHTHAT(#rope{ length = L }, gen_rope(), L =/= 0).

gen_valid_index(#rope{ length = L }) ->
    integer(0, L).

gen_valid_position(#rope{ length = L }) ->
    integer(1, L).

