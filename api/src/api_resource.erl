-module(api_resource).
-export([init/1,
         uri_too_long/2,
         malformed_request/2,
         resource_exists/2,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2,
         parse_query/1
        ]).

-include_lib("webmachine/include/webmachine.hrl").

%% This function is called after Dispatch
init([]) ->
    io:format("init~n"),
    {ok, maps:new()}.
%% {{trace, "/tmp"}, maps:new()}. %% Debug

%% function that checks if the request is too long, (if uri_too_long
%% -> true, else false)
uri_too_long(ReqData, State) ->
    {wrq:path_info(ReqData) < 3, ReqData, State#{ length => length(wrq:path_info(ReqData)) }}.

%% function that defines the allowed methods to the API (GET, PUT,
%% POST, DELETE, HEAD...)
allowed_methods(ReqData, State) ->
    {['GET', 'HEAD'], ReqData, State}.

%% function that checks if a function is malformed (if malformed ->
%% true, else false)
malformed_request(ReqData, State) ->
    io:format("malformed_request:~p~n", [State]),
    Method = wrq:method(ReqData),
    Length = maps:get(length, State),
    case lists:keyfind(op, 1, wrq:path_info(ReqData)) of
        {op, "price"} when (Length == 1) and (Method == 'GET') ->
            {false, ReqData, State#{ op => price }};
        {op, "price"} when (Length == 2) and (Method == 'GET') ->
            {false, ReqData, State#{ op => price, id => get_from_path(ReqData, id) }};
        {op, "quantity"} when (Length == 1) and (Method == 'GET') ->
            {false, ReqData, State#{ op => quantity }};
        {op, "quantity"} when (Length == 2) and (Method == 'GET') ->
            {false, ReqData, State#{ op => quantity, id => get_from_path(ReqData, id) }};
        false when (Length == 0) and (Method == 'GET') ->
            {false, ReqData, State#{ op => info }};
        false when (Length == 1) and (Method == 'GET') ->
            io:format("hola"),
            {false, ReqData, State#{ op => info, id => get_from_path(ReqData, id) }};
        _ ->
            {true, ReqData, State}
    end.

%% function that checks if the counter of a request exists
resource_exists(ReqData, State) ->
    io:format("resource_exists:~p~n", [State]),
    {lists:foldl(fun(X, Acc) ->
                         case maps:get(X, State) of
                             error ->
                                 false and Acc;
                             Value when X == id ->
                                 db_client:exists(Value) and Acc;
                             _ -> true and Acc
                         end
                 end, true, maps:keys(State)), ReqData, State}.

%% function that gives the content types that the API provides (html,
%% json...)
content_types_provided(ReqData, State) ->
    io:format("content_types_provided:~p~n", [State]),
    {[{"application/json", to_json} ], ReqData, State}.

%% function that creates the json response
to_json(ReqData, State) ->
    io:format("to_json:~p~n", [State]),
    Length = maps:get(length, State),
    Json =
        case maps:get(op, State) of
            info when Length == 0 ->
                {struct, parse_query(db_client:all_info())};
            info when Length == 1 ->
                {struct, parse_query(db_client:product_info(maps:get(id, State)))};
            price when Length == 1 ->
                {struct, parse_query(db_client:prices())};
            price when Length == 2 ->
                {struct, parse_query(db_client:price(maps:get(id, State)))};
            quantity when Length == 1 ->
                {struct, parse_query(db_client:quantity())};
            quantity when Length == 2 ->
                {struct, parse_query(db_client:quantity(maps:get(id, State)))}
        end,
    io:format("JSON : ~p~n", [Json]),
    {mochijson2:encode(Json), ReqData, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_from_path(ReqData, Key) ->
    case lists:keyfind(Key, 1, wrq:path_info(ReqData)) of
        {Key, Id} -> Id;
        _ -> error
    end.

parse_query({error, _}) -> [{result, "nothing to show"}];
parse_query({selected, Columns, Results}) ->
    Values =
        lists:map(fun(X) ->
                          lists:map(fun({Column, N}) -> {Column, element(N, X)} end,
                                    lists:zip(Columns, lists:seq(1, length(Columns))))
                  end, Results),
    case length(Values) of
        1 -> lists:flatten(Values);
        _ -> [{info, {array, Values}}]
    end.
