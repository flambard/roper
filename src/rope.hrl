-include("rope_tree.hrl").

-type rope_tree() :: string() | ?NODE(integer(), rope_tree(), rope_tree()).

-record(rope,
        { length     = 0  :: non_neg_integer(),
          strictness = 1  :: pos_integer(),
          tree       = "" :: rope_tree()
        }).
