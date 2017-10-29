-module(beamup_util).

-export([override/4, override/6]).

override(Key, N, TupleList, NewTuple) ->
    case lists:keyfind(Key, N, TupleList) of
      false -> TupleList ++ [NewTuple];
      {Key, List} when is_list(List) ->
        MergedList = lists:usort(lists:merge(List, element(2, NewTuple))),
        lists:keyreplace(Key, N, TupleList, {Key, MergedList});
      _ ->
        lists:keyreplace(Key, N, TupleList, NewTuple)
    end.
  
override(Key1, N1, Key2, N2, TupleList, NewTuple) ->
  case lists:keyfind(Key1, N1, TupleList) of
    false -> {Key1, [NewTuple]};
    {Key1, TupleList2} ->
      MergedTupleList = override(Key2, N2, TupleList2, NewTuple),
      lists:keyreplace(Key1, N1, TupleList, {Key1, MergedTupleList})
  end.
